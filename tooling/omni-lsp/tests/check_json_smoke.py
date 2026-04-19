#!/usr/bin/env python3
from __future__ import annotations

import json
import os
import subprocess
import tempfile
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[3]
OMNI = ROOT / "build" / "main"


def expect(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def expect_payload_schema(payload: dict[str, Any], path: Path) -> None:
    expect(set(payload.keys()) == {"ok", "path", "diagnostics"}, f"unexpected payload keys: {sorted(payload.keys())}")
    expect(payload["path"] == str(path.resolve()), f"unexpected path for {path.name}: {payload['path']!r}")
    expect(isinstance(payload["ok"], bool), f"expected bool ok field for {path.name}")
    expect(isinstance(payload["diagnostics"], list), f"expected diagnostics list for {path.name}")


def expect_diagnostic_schema(diagnostic: dict[str, Any]) -> None:
    expect(
        set(diagnostic.keys()) == {"code", "severity", "message", "range"},
        f"unexpected diagnostic keys: {sorted(diagnostic.keys())}",
    )
    expect(isinstance(diagnostic["code"], str), "diagnostic code must be a string")
    expect(isinstance(diagnostic["severity"], str), "diagnostic severity must be a string")
    expect(isinstance(diagnostic["message"], str), "diagnostic message must be a string")
    expect(isinstance(diagnostic["range"], dict), "diagnostic range must be an object")

    range_payload = diagnostic["range"]
    expect(set(range_payload.keys()) == {"start", "end"}, f"unexpected range keys: {sorted(range_payload.keys())}")
    for key in ("start", "end"):
        point = range_payload[key]
        expect(isinstance(point, dict), f"range {key} must be an object")
        expect(set(point.keys()) == {"line", "character"}, f"unexpected {key} keys: {sorted(point.keys())}")
        expect(isinstance(point["line"], int), f"{key}.line must be an int")
        expect(isinstance(point["character"], int), f"{key}.character must be an int")


def run_check(path: Path) -> tuple[int, dict[str, Any]]:
    env = os.environ.copy()
    env.setdefault("LD_LIBRARY_PATH", "/usr/local/lib")
    result = subprocess.run(
        [str(OMNI), "--check", "--json", str(path)],
        capture_output=True,
        text=True,
        check=False,
        env=env,
    )
    expect(result.stdout != "", f"expected JSON stdout for {path.name}")
    expect(result.stderr == "", f"unexpected stderr for {path.name}: {result.stderr!r}")
    payload = json.loads(result.stdout)
    expect_payload_schema(payload, path)
    return result.returncode, payload


def run_cli(*args: str, env_overrides: dict[str, str] | None = None) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env.setdefault("LD_LIBRARY_PATH", "/usr/local/lib")
    if env_overrides:
        env.update(env_overrides)
    return subprocess.run(
        [str(OMNI), *args],
        capture_output=True,
        text=True,
        check=False,
        env=env,
        cwd=ROOT,
    )


def main() -> int:
    expect(OMNI.exists(), f"missing Omni binary at {OMNI}")

    with tempfile.TemporaryDirectory(prefix="omni_check_json_") as tempdir:
        temp_root = Path(tempdir)

        clean_path = temp_root / "clean.omni"
        clean_path.write_text("(define answer 42)\n", encoding="utf-8")

        syntax_path = temp_root / "syntax.omni"
        syntax_path.write_text("(define answer 42\n", encoding="utf-8")

        compile_syntax_path = temp_root / "compile_syntax.omni"
        compile_syntax_path.write_text("(define answer 42", encoding="utf-8")

        lowering_path = temp_root / "lowering.omni"
        lowering_path.write_text('(define [ffi lib] c "/tmp/libx.so")\n', encoding="utf-8")

        module_path = temp_root / "module_bad.omni"
        module_path.write_text("(module)\n", encoding="utf-8")

        import_path = temp_root / "import_bad.omni"
        import_path.write_text("(import)\n", encoding="utf-8")

        clean_rc, clean_payload = run_check(clean_path)
        expect(clean_rc == 0, f"clean input returned {clean_rc}")
        expect(clean_payload["ok"] is True, "clean input should be ok")
        expect(clean_payload["diagnostics"] == [], "clean input should not report diagnostics")

        syntax_rc, syntax_payload = run_check(syntax_path)
        expect(syntax_rc == 1, f"syntax error returned {syntax_rc}")
        expect(syntax_payload["ok"] is False, "syntax error should not be ok")
        expect(len(syntax_payload["diagnostics"]) == 1, "syntax error should report one diagnostic")
        syntax_diagnostic = syntax_payload["diagnostics"][0]
        expect_diagnostic_schema(syntax_diagnostic)
        expect(syntax_diagnostic["code"] == "parser/syntax-error", "unexpected syntax diagnostic code")
        expect(syntax_diagnostic["severity"] == "error", "unexpected syntax severity")
        expect(syntax_diagnostic["message"] == "expected ')'", "unexpected syntax diagnostic message")
        expect(
            syntax_diagnostic["range"]
            == {
                "start": {"line": 1, "character": 0},
                "end": {"line": 1, "character": 1},
            },
            f"unexpected syntax range: {syntax_diagnostic['range']!r}",
        )

        compile_out = temp_root / "compile_syntax.c3"
        compile_result = run_cli("--compile", str(compile_syntax_path), str(compile_out))
        expect(compile_result.returncode == 1, f"malformed --compile returned {compile_result.returncode}")
        expect(compile_result.stderr == "", f"unexpected stderr for malformed --compile: {compile_result.stderr!r}")
        expect(
            compile_result.stdout
            == f"Compiling {compile_syntax_path} to {compile_out}...\n"
            "Error: compiler syntax error at line 1, column 18: expected ')'\n",
            f"unexpected malformed --compile output: {compile_result.stdout!r}",
        )
        expect(not compile_out.exists(), "malformed --compile should not leave an output file")

        build_out = temp_root / "compile_syntax_bin"
        build_result = run_cli("--build", str(compile_syntax_path), "-o", str(build_out))
        expect(build_result.returncode == 1, f"malformed --build returned {build_result.returncode}")
        expect(build_result.stderr == "", f"unexpected stderr for malformed --build: {build_result.stderr!r}")
        expect(
            build_result.stdout
            == f"Compiling {compile_syntax_path}...\n"
            "Error: compiler syntax error at line 1, column 18: expected ')'\n",
            f"unexpected malformed --build output: {build_result.stdout!r}",
        )
        expect(not build_out.exists(), "malformed --build should not leave an output file")

        missing_path = temp_root / "missing.omni"
        missing_compile_out = temp_root / "missing.c3"
        missing_compile = run_cli("--compile", str(missing_path), str(missing_compile_out))
        expect(missing_compile.returncode == 1, f"missing-file --compile returned {missing_compile.returncode}")
        expect(missing_compile.stderr == "", f"unexpected stderr for missing-file --compile: {missing_compile.stderr!r}")
        expect(
            missing_compile.stdout
            == f"Error: cannot read input file '{missing_path}': file not found\n",
            f"unexpected missing-file --compile output: {missing_compile.stdout!r}",
        )
        expect(not missing_compile_out.exists(), "missing-file --compile should not leave an output file")

        missing_build_out = temp_root / "missing_bin"
        missing_build = run_cli("--build", str(missing_path), "-o", str(missing_build_out))
        expect(missing_build.returncode == 1, f"missing-file --build returned {missing_build.returncode}")
        expect(missing_build.stderr == "", f"unexpected stderr for missing-file --build: {missing_build.stderr!r}")
        expect(
        missing_build.stdout
            == f"Error: cannot read input file '{missing_path}': file not found\n",
            f"unexpected missing-file --build output: {missing_build.stdout!r}",
        )
        expect(not missing_build_out.exists(), "missing-file --build should not leave an output file")

        invalid_output = temp_root / "missing-dir" / "out.c3"
        invalid_output_compile = run_cli("--compile", str(clean_path), str(invalid_output))
        expect(
            invalid_output_compile.returncode == 1,
            f"invalid-output --compile returned {invalid_output_compile.returncode}",
        )
        expect(
            invalid_output_compile.stderr == "",
            f"unexpected stderr for invalid-output --compile: {invalid_output_compile.stderr!r}",
        )
        expect(
            invalid_output_compile.stdout
            == f"Error: cannot write output file '{invalid_output}': invalid path\n",
            f"unexpected invalid-output --compile output: {invalid_output_compile.stdout!r}",
        )
        expect(not invalid_output.exists(), "invalid-output --compile should not leave an output file")

        blocked_compile_dir = temp_root / "blocked-compile-dir"
        blocked_compile_dir.mkdir()
        blocked_compile_dir.chmod(0o555)
        try:
            blocked_compile_out = blocked_compile_dir / "out.c3"
            blocked_compile = run_cli("--compile", str(clean_path), str(blocked_compile_out))
            expect(
                blocked_compile.returncode == 1,
                f"blocked-output --compile returned {blocked_compile.returncode}",
            )
            expect(
                blocked_compile.stderr == "",
                f"unexpected stderr for blocked-output --compile: {blocked_compile.stderr!r}",
            )
            expect(
                blocked_compile.stdout
                == f"Error: cannot write output file '{blocked_compile_out}': permission denied\n",
                f"unexpected blocked-output --compile output: {blocked_compile.stdout!r}",
            )
        finally:
            blocked_compile_dir.chmod(0o755)

        invalid_build_output = temp_root / "missing-build-dir" / "outbin"
        invalid_output_build = run_cli("--build", str(clean_path), "-o", str(invalid_build_output))
        expect(
            invalid_output_build.returncode == 1,
            f"invalid-output --build returned {invalid_output_build.returncode}",
        )
        expect(
            invalid_output_build.stderr == "",
            f"unexpected stderr for invalid-output --build: {invalid_output_build.stderr!r}",
        )
        expect(
            invalid_output_build.stdout
            == f"Error: cannot write output binary '{invalid_build_output}': invalid path\n",
            f"unexpected invalid-output --build output: {invalid_output_build.stdout!r}",
        )
        expect(not invalid_build_output.exists(), "invalid-output --build should not leave an output file")

        directory_output = temp_root / "existing-dir"
        directory_output.mkdir()
        directory_output_build = run_cli("--build", str(clean_path), "-o", str(directory_output))
        expect(
            directory_output_build.returncode == 1,
            f"directory-output --build returned {directory_output_build.returncode}",
        )
        expect(
            directory_output_build.stderr == "",
            f"unexpected stderr for directory-output --build: {directory_output_build.stderr!r}",
        )
        expect(
            directory_output_build.stdout
            == f"Error: cannot write output binary '{directory_output}': invalid path\n",
            f"unexpected directory-output --build output: {directory_output_build.stdout!r}",
        )

        blocked_output_dir = temp_root / "blocked-dir"
        blocked_output_dir.mkdir()
        blocked_output_dir.chmod(0o555)
        try:
            blocked_output = blocked_output_dir / "outbin"
            blocked_output_build = run_cli("--build", str(clean_path), "-o", str(blocked_output))
            expect(
                blocked_output_build.returncode == 1,
                f"blocked-output --build returned {blocked_output_build.returncode}",
            )
            expect(
                blocked_output_build.stderr == "",
                f"unexpected stderr for blocked-output --build: {blocked_output_build.stderr!r}",
            )
            expect(
                blocked_output_build.stdout
                == f"Error: cannot write output binary '{blocked_output}': permission denied\n",
                f"unexpected blocked-output --build output: {blocked_output_build.stdout!r}",
            )
        finally:
            blocked_output_dir.chmod(0o755)

        missing_c3c = temp_root / "missing-c3c"
        backend_spawn_build = run_cli(
            "--build",
            str(clean_path),
            "-o",
            str(temp_root / "backend-spawn-bin"),
            env_overrides={"C3C": str(missing_c3c)},
        )
        expect(
            backend_spawn_build.returncode == 1,
            f"backend-spawn --build returned {backend_spawn_build.returncode}",
        )
        expect(
            backend_spawn_build.stdout
            == f"Compiling {clean_path}...\n"
            f"Building {temp_root / 'backend-spawn-bin'}...\n",
            f"unexpected backend-spawn --build stdout: {backend_spawn_build.stdout!r}",
        )
        expect(
            "c3c compilation failed while compiling" not in backend_spawn_build.stdout,
            f"backend-spawn --build should not print the generic backend wrapper: {backend_spawn_build.stdout!r}",
        )
        expect(
            backend_spawn_build.stderr
            == f"Error: missing AOT backend compiler at '{missing_c3c}'\n",
            f"unexpected backend-spawn --build stderr: {backend_spawn_build.stderr!r}",
        )
        expect(
            "build/_aot_temp_" not in backend_spawn_build.stdout,
            f"backend-spawn --build should not leak the temp C3 path: {backend_spawn_build.stdout!r}",
        )
        expect(not (temp_root / "backend-spawn-bin").exists(), "backend-spawn --build should not leave an output file")

        lowering_rc, lowering_payload = run_check(lowering_path)
        expect(lowering_rc == 1, f"lowering error returned {lowering_rc}")
        expect(lowering_payload["ok"] is False, "lowering error should not be ok")
        expect(len(lowering_payload["diagnostics"]) == 1, "lowering error should report one diagnostic")
        lowering_diagnostic = lowering_payload["diagnostics"][0]
        expect_diagnostic_schema(lowering_diagnostic)
        expect(lowering_diagnostic["code"] == "compiler/lowering-error", "unexpected lowering diagnostic code")
        expect(lowering_diagnostic["severity"] == "error", "unexpected lowering severity")
        expect(
            lowering_diagnostic["message"] == "compiler: [ffi lib] is not supported by AOT lowering",
            f"unexpected lowering diagnostic message: {lowering_diagnostic['message']!r}",
        )
        expect(
            lowering_diagnostic["range"]
            == {
                "start": {"line": 0, "character": 0},
                "end": {"line": 0, "character": 1},
            },
            f"unexpected lowering range: {lowering_diagnostic['range']!r}",
        )

        module_rc, module_payload = run_check(module_path)
        expect(module_rc == 1, f"malformed module returned {module_rc}")
        expect(module_payload["ok"] is False, "malformed module should not be ok")
        expect(len(module_payload["diagnostics"]) == 1, "malformed module should report one diagnostic")
        module_diagnostic = module_payload["diagnostics"][0]
        expect_diagnostic_schema(module_diagnostic)
        expect(module_diagnostic["code"] == "parser/syntax-error", "unexpected module diagnostic code")
        expect(module_diagnostic["message"] == "expected module name after module", "unexpected module diagnostic message")
        expect(
            module_diagnostic["range"]
            == {
                "start": {"line": 0, "character": 7},
                "end": {"line": 0, "character": 8},
            },
            f"unexpected module range: {module_diagnostic['range']!r}",
        )

        import_rc, import_payload = run_check(import_path)
        expect(import_rc == 1, f"malformed import returned {import_rc}")
        expect(import_payload["ok"] is False, "malformed import should not be ok")
        expect(len(import_payload["diagnostics"]) == 1, "malformed import should report one diagnostic")
        import_diagnostic = import_payload["diagnostics"][0]
        expect_diagnostic_schema(import_diagnostic)
        expect(import_diagnostic["code"] == "parser/syntax-error", "unexpected import diagnostic code")
        expect(import_diagnostic["message"] == "expected module name or path after import", "unexpected import diagnostic message")
        expect(
            import_diagnostic["range"]
            == {
                "start": {"line": 0, "character": 7},
                "end": {"line": 0, "character": 8},
            },
            f"unexpected import range: {import_diagnostic['range']!r}",
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
