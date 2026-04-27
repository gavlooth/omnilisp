#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
OMNI = ROOT / "build" / "main"


def expect(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def run_fmt(*args: str) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env.setdefault("LD_LIBRARY_PATH", "/usr/local/lib")
    return subprocess.run(
        [str(OMNI), "--fmt", *args],
        capture_output=True,
        text=True,
        check=False,
        env=env,
    )


def read_text_raw(path: Path) -> str:
    with path.open("r", encoding="utf-8", newline="") as handle:
        return handle.read()


def run_fmt_raw(*args: str) -> subprocess.CompletedProcess[bytes]:
    env = os.environ.copy()
    env.setdefault("LD_LIBRARY_PATH", "/usr/local/lib")
    return subprocess.run(
        [str(OMNI), "--fmt", *args],
        capture_output=True,
        text=False,
        check=False,
        env=env,
    )


def main() -> int:
    expect(OMNI.exists(), f"missing Omni binary at {OMNI}")

    original = (
        "(module demo\n"
        " (define (ping x)\n"
        "   (+ x 1))   \n"
        "\n"
        "  [1\n"
        "   {'name \"a\"   }\n"
        "   (handle\n"
        "    (signal 'ask 41)\n"
        "      (ask value\n"
        "     (resolve (+ value 1))))])\n"
    )
    expected = (
        "(module demo\n"
        "  (define (ping x)\n"
        "    (+ x 1))\n"
        "\n"
        "  [1\n"
        "    {'name \"a\"   }\n"
        "    (handle\n"
        "      (signal 'ask 41)\n"
        "      (ask value\n"
        "           (resolve (+ value 1))))])\n"
    )
    continuation_original = (
        "(module sample\n"
        "(export alpha beta gamma\n"
        "delta epsilon)\n"
        "(define (pair-sum pairs)\n"
        "(let (left (car pairs)\n"
        "right (cadr pairs))\n"
        "(+ left right))))\n"
    )
    continuation_expected = (
        "(module sample\n"
        "  (export alpha beta gamma\n"
        "          delta epsilon)\n"
        "  (define (pair-sum pairs)\n"
        "    (let (left (car pairs)\n"
        "          right (cadr pairs))\n"
        "      (+ left right))))\n"
    )
    branch_original = (
        "(if (= 1 1)\n"
        "100\n"
        "200)\n"
        "\n"
        "(let ^rec (fact (lambda (n)\n"
        "(if (= n 0)\n"
        "1\n"
        "(* n (fact (- n 1))))))\n"
        "(fact 5))\n"
    )
    branch_expected = (
        "(if (= 1 1)\n"
        "    100\n"
        "    200)\n"
        "\n"
        "(let ^rec (fact (lambda (n)\n"
        "                  (if (= n 0)\n"
        "                      1\n"
        "                      (* n (fact (- n 1))))))\n"
        "  (fact 5))\n"
    )
    generic_original = (
        "(define (pipeline raw)\n"
        "(|> (tcp-read raw)\n"
        "decode-request\n"
        "execute\n"
        "respond))\n"
    )
    generic_expected = (
        "(define (pipeline raw)\n"
        "  (|> (tcp-read raw)\n"
        "      decode-request\n"
        "      execute\n"
        "      respond))\n"
    )
    nested_if_original = (
        "(match method\n"
        "(\"GET\"    (if ready\n"
        "(let (item (repo/get-item ready))\n"
        "(if item (Ok item) (Err \"missing\")))\n"
        "(if (= method \"/items\")\n"
        "(Ok (repo/list))\n"
        "(Err \"route not found\"))))\n"
        "(_        (Err \"route not found\")))\n"
    )
    nested_if_expected = (
        "(match method\n"
        "  (\"GET\"    (if ready\n"
        "                (let (item (repo/get-item ready))\n"
        "                  (if item (Ok item) (Err \"missing\")))\n"
        "                (if (= method \"/items\")\n"
        "                    (Ok (repo/list))\n"
        "                    (Err \"route not found\"))))\n"
        "  (_        (Err \"route not found\")))\n"
    )
    control_forms_original = (
        "(define (guarded retry value)\n"
        "(let (result (checkpoint\n"
        "(unless retry\n"
        "(raise err\n"
        "(recover err)))))\n"
        "(when value\n"
        "(emit value))\n"
        "result))\n"
    )
    control_forms_expected = (
        "(define (guarded retry value)\n"
        "  (let (result (checkpoint\n"
        "    (unless retry\n"
        "      (raise err\n"
        "        (recover err)))))\n"
        "    (when value\n"
        "      (emit value))\n"
        "    result))\n"
    )
    higher_order_original = (
        "(define (collect prices)\n"
        "(foldl (lambda (acc price)\n"
        "(let (bucket (map (lambda (x)\n"
        "(* x price))\n"
        "acc))\n"
        "(cons bucket acc)))\n"
        "nil\n"
        "prices))\n"
    )
    higher_order_expected = (
        "(define (collect prices)\n"
        "  (foldl (lambda (acc price)\n"
        "           (let (bucket (map (lambda (x)\n"
        "                               (* x price))\n"
        "                             acc))\n"
        "             (cons bucket acc)))\n"
        "         nil\n"
        "         prices))\n"
    )
    coroutine_original = (
        "(define (make-worker seed)\n"
        "(Coroutine (lambda ()\n"
        "(let loop (n seed)\n"
        "(if (= n 0)\n"
        "n\n"
        "(block\n"
        "(yield n)\n"
        "(loop (- n 1)))))))\n"
    )
    coroutine_expected = (
        "(define (make-worker seed)\n"
        "  (Coroutine (lambda ()\n"
        "      (let loop (n seed)\n"
        "        (if (= n 0)\n"
        "            n\n"
        "            (block\n"
        "              (yield n)\n"
        "              (loop (- n 1)))))))\n"
    )
    clause_data_original = (
        "(define (summarize result pair)\n"
        "(let (payload (match result\n"
        "((Ok value)\n"
        "(let (scaled (+ value 1))\n"
        "(Ok scaled)))\n"
        "((Err msg) (Err msg))))\n"
        "(let ([left right] pair)\n"
        "{'payload payload\n"
        "'window [(- right left)\n"
        "(+ left right)]\n"
        "'total (+ left right)})))\n"
    )
    clause_data_expected = (
        "(define (summarize result pair)\n"
        "  (let (payload (match result\n"
        "                  ((Ok value)\n"
        "                   (let (scaled (+ value 1))\n"
        "                     (Ok scaled)))\n"
        "                  ((Err msg) (Err msg))))\n"
        "    (let ([left right] pair)\n"
        "      {'payload payload\n"
        "       'window [(- right left)\n"
        "                (+ left right)]\n"
        "       'total (+ left right)})))\n"
    )
    crlf_original = (
        "(module windows\r\n"
        "(export alpha beta\r\n"
        "gamma)\r\n"
    )
    crlf_expected = (
        "(module windows\r\n"
        "  (export alpha beta\r\n"
        "          gamma)\r\n"
    )

    with tempfile.TemporaryDirectory(prefix="omni_fmt_") as tempdir:
        path = Path(tempdir) / "sample.omni"
        path.write_text(original, encoding="utf-8")

        result = run_fmt(str(path))
        expect(result.returncode == 0, f"--fmt stdout returned {result.returncode}")
        expect(result.stderr == "", f"unexpected stderr for stdout mode: {result.stderr!r}")
        expect(result.stdout == expected, "stdout formatting output mismatch")
        expect(path.read_text(encoding="utf-8") == original, "stdout mode should not rewrite the file")

        check_before = run_fmt("--check", str(path))
        expect(check_before.returncode == 1, f"--fmt --check before write returned {check_before.returncode}")
        expect(check_before.stdout == "", "--fmt --check should not print stdout")
        expect(check_before.stderr == "", "--fmt --check should not print stderr")

        write_result = run_fmt("--write", str(path))
        expect(write_result.returncode == 0, f"--fmt --write returned {write_result.returncode}")
        expect(write_result.stdout == "", "--fmt --write should not print stdout")
        expect(write_result.stderr == "", "--fmt --write should not print stderr")
        expect(path.read_text(encoding="utf-8") == expected, "--fmt --write did not rewrite the file")

        check_after = run_fmt("--check", str(path))
        expect(check_after.returncode == 0, f"--fmt --check after write returned {check_after.returncode}")
        expect(check_after.stdout == "", "--fmt --check after write should not print stdout")
        expect(check_after.stderr == "", "--fmt --check after write should not print stderr")

        continuation_path = Path(tempdir) / "continuations.omni"
        continuation_path.write_text(continuation_original, encoding="utf-8")

        continuation_result = run_fmt(str(continuation_path))
        expect(continuation_result.returncode == 0, f"--fmt continuation stdout returned {continuation_result.returncode}")
        expect(continuation_result.stderr == "", f"unexpected stderr for continuation stdout mode: {continuation_result.stderr!r}")
        expect(continuation_result.stdout == continuation_expected, "continuation formatting output mismatch")
        expect(
            continuation_path.read_text(encoding="utf-8") == continuation_original,
            "stdout mode should not rewrite the continuation file",
        )

        continuation_write = run_fmt("--write", str(continuation_path))
        expect(continuation_write.returncode == 0, f"--fmt --write continuation returned {continuation_write.returncode}")
        expect(
            continuation_path.read_text(encoding="utf-8") == continuation_expected,
            "--fmt --write did not rewrite continuation formatting",
        )

        branch_path = Path(tempdir) / "branching.omni"
        branch_path.write_text(branch_original, encoding="utf-8")

        branch_result = run_fmt(str(branch_path))
        expect(branch_result.returncode == 0, f"--fmt branch stdout returned {branch_result.returncode}")
        expect(branch_result.stderr == "", f"unexpected stderr for branch stdout mode: {branch_result.stderr!r}")
        expect(branch_result.stdout == branch_expected, "branch formatting output mismatch")

        branch_write = run_fmt("--write", str(branch_path))
        expect(branch_write.returncode == 0, f"--fmt --write branch returned {branch_write.returncode}")
        expect(
            branch_path.read_text(encoding="utf-8") == branch_expected,
            "--fmt --write did not preserve branch and recursive-let indentation",
        )

        generic_path = Path(tempdir) / "generic.omni"
        generic_path.write_text(generic_original, encoding="utf-8")

        generic_result = run_fmt(str(generic_path))
        expect(generic_result.returncode == 0, f"--fmt generic stdout returned {generic_result.returncode}")
        expect(generic_result.stderr == "", f"unexpected stderr for generic stdout mode: {generic_result.stderr!r}")
        expect(generic_result.stdout == generic_expected, "generic call formatting output mismatch")

        generic_write = run_fmt("--write", str(generic_path))
        expect(generic_write.returncode == 0, f"--fmt --write generic returned {generic_write.returncode}")
        expect(
            generic_path.read_text(encoding="utf-8") == generic_expected,
            "--fmt --write did not preserve generic call continuation alignment",
        )

        nested_if_path = Path(tempdir) / "nested_if.omni"
        nested_if_path.write_text(nested_if_original, encoding="utf-8")

        nested_if_result = run_fmt(str(nested_if_path))
        expect(nested_if_result.returncode == 0, f"--fmt nested if stdout returned {nested_if_result.returncode}")
        expect(nested_if_result.stderr == "", f"unexpected stderr for nested if stdout mode: {nested_if_result.stderr!r}")
        expect(nested_if_result.stdout == nested_if_expected, "nested if formatting output mismatch")

        nested_if_write = run_fmt("--write", str(nested_if_path))
        expect(nested_if_write.returncode == 0, f"--fmt --write nested if returned {nested_if_write.returncode}")
        expect(
            nested_if_path.read_text(encoding="utf-8") == nested_if_expected,
            "--fmt --write did not preserve nested if branch alignment",
        )

        control_forms_path = Path(tempdir) / "control_forms.omni"
        control_forms_path.write_text(control_forms_original, encoding="utf-8")

        control_forms_result = run_fmt(str(control_forms_path))
        expect(
            control_forms_result.returncode == 0,
            f"--fmt control forms stdout returned {control_forms_result.returncode}",
        )
        expect(
            control_forms_result.stderr == "",
            f"unexpected stderr for control forms stdout mode: {control_forms_result.stderr!r}",
        )
        expect(control_forms_result.stdout == control_forms_expected, "control forms formatting output mismatch")

        control_forms_write = run_fmt("--write", str(control_forms_path))
        expect(
            control_forms_write.returncode == 0,
            f"--fmt --write control forms returned {control_forms_write.returncode}",
        )
        expect(
            control_forms_path.read_text(encoding="utf-8") == control_forms_expected,
            "--fmt --write did not preserve control-form block indentation",
        )

        higher_order_path = Path(tempdir) / "higher_order.omni"
        higher_order_path.write_text(higher_order_original, encoding="utf-8")

        higher_order_result = run_fmt(str(higher_order_path))
        expect(
            higher_order_result.returncode == 0,
            f"--fmt higher-order stdout returned {higher_order_result.returncode}",
        )
        expect(
            higher_order_result.stderr == "",
            f"unexpected stderr for higher-order stdout mode: {higher_order_result.stderr!r}",
        )
        expect(higher_order_result.stdout == higher_order_expected, "higher-order lambda formatting output mismatch")

        higher_order_write = run_fmt("--write", str(higher_order_path))
        expect(
            higher_order_write.returncode == 0,
            f"--fmt --write higher-order returned {higher_order_write.returncode}",
        )
        expect(
            higher_order_path.read_text(encoding="utf-8") == higher_order_expected,
            "--fmt --write did not preserve higher-order lambda-body alignment",
        )

        coroutine_path = Path(tempdir) / "coroutine.omni"
        coroutine_path.write_text(coroutine_original, encoding="utf-8")

        coroutine_result = run_fmt(str(coroutine_path))
        expect(
            coroutine_result.returncode == 0,
            f"--fmt coroutine stdout returned {coroutine_result.returncode}",
        )
        expect(
            coroutine_result.stderr == "",
            f"unexpected stderr for coroutine stdout mode: {coroutine_result.stderr!r}",
        )
        expect(coroutine_result.stdout == coroutine_expected, "coroutine wrapper formatting output mismatch")

        coroutine_write = run_fmt("--write", str(coroutine_path))
        expect(
            coroutine_write.returncode == 0,
            f"--fmt --write coroutine returned {coroutine_write.returncode}",
        )
        expect(
            coroutine_path.read_text(encoding="utf-8") == coroutine_expected,
            "--fmt --write did not preserve coroutine wrapper lambda-body alignment",
        )

        clause_data_path = Path(tempdir) / "clause_data.omni"
        clause_data_path.write_text(clause_data_original, encoding="utf-8")

        clause_data_result = run_fmt(str(clause_data_path))
        expect(
            clause_data_result.returncode == 0,
            f"--fmt clause/data stdout returned {clause_data_result.returncode}",
        )
        expect(
            clause_data_result.stderr == "",
            f"unexpected stderr for clause/data stdout mode: {clause_data_result.stderr!r}",
        )
        expect(clause_data_result.stdout == clause_data_expected, "clause/data formatting output mismatch")

        clause_data_write = run_fmt("--write", str(clause_data_path))
        expect(
            clause_data_write.returncode == 0,
            f"--fmt --write clause/data returned {clause_data_write.returncode}",
        )
        expect(
            clause_data_path.read_text(encoding="utf-8") == clause_data_expected,
            "--fmt --write did not preserve clause and inline data-layout indentation",
        )

        crlf_path = Path(tempdir) / "windows.omni"
        crlf_path.write_text(crlf_original, encoding="utf-8", newline="")

        crlf_result = run_fmt_raw(str(crlf_path))
        expect(crlf_result.returncode == 0, f"--fmt crlf stdout returned {crlf_result.returncode}")
        expect(crlf_result.stderr == b"", f"unexpected stderr for crlf stdout mode: {crlf_result.stderr!r}")
        expect(crlf_result.stdout == crlf_expected.encode("utf-8"), "CRLF formatting output mismatch")

        crlf_write = run_fmt("--write", str(crlf_path))
        expect(crlf_write.returncode == 0, f"--fmt --write crlf returned {crlf_write.returncode}")
        expect(read_text_raw(crlf_path) == crlf_expected, "--fmt --write did not preserve CRLF newlines")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
