module lisp;

import std::core::mem;
import std::io;

fn void run_ml_visualization_export_image_atomic_tests(Interp* interp, int* pass, int* fail) {
    char[] final_path = "/tmp/";
    char[] temp_path = "";
    char* temp_heap = null;

    if (!ml_image_build_atomic_temp_path(final_path, &temp_path, &temp_heap)) {
        io::printn("[FAIL] ml/export-image atomic rename failure test could not build temp path");
        (*fail)++;
        return;
    }
    defer mem::free(temp_heap);
    defer (void)omni_uv_fs_unlink((ZString)temp_path.ptr);

    (void)omni_uv_fs_unlink((ZString)temp_path.ptr);
    EvalResult result = run("(ml/export-image (Tensor Float32 [1 1] [1]) \"/tmp/\")", interp);
    bool ok = result.error.has_error &&
        io::file::is_dir((String)final_path) &&
        !io::file::exists((String)temp_path);

    if (ok) {
        emit_pass_literal("[PASS] ml/export-image cleans temp output after rename failure");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] ml/export-image cleans temp output after rename failure (error=%s final_dir=%s temp_exists=%s)",
            result.error.has_error ? "yes" : "no",
            io::file::is_dir((String)final_path) ? "yes" : "no",
            io::file::exists((String)temp_path) ? "yes" : "no"
        );
        (*fail)++;
    }
}
