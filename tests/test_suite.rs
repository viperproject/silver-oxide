use std::path::PathBuf;

#[test]
fn viper_testsuite() {
    const TEST_SUITE_DIR: &str = "/Users/jfiala/Documents/GitHub/viper/viperserver/silicon/silver/src/test/resources";
    let mut all_vpr_files = Vec::new();
    get_all_vpr_files(PathBuf::from(TEST_SUITE_DIR), &mut all_vpr_files);
    all_vpr_files.sort();

    for vpr_file in all_vpr_files {
        let contents = std::fs::read_to_string(&vpr_file).unwrap();
        if contents.contains("ExpectedOutput(") {
            println!("Skipping {} as it contains `ExpectedOutput`", vpr_file.display());
            continue;
        } else {
            println!("Processing {}", vpr_file.display());
        }
        silver_oxide::full(&contents).unwrap();
    }
}

fn get_all_vpr_files(path: PathBuf, files: &mut Vec<PathBuf>) {
    if !path.is_dir() {
        if path.extension().is_some_and(|ext| ext == "vpr") {
            files.push(path);
        }
        return;
    }
    for entry in std::fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        get_all_vpr_files(entry.path(), files);
    }
}
