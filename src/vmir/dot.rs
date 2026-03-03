use crate::log_dir;

use super::global::members::DepGraph;

pub(crate) trait CanDot {
    fn dump_dot(&self, force: bool) -> Option<String> {
        if !force && std::env::var("VIPER_DOT").is_err() {
            return None;
        }
        let mut path = log_dir();
        path += "/";
        path += &self.filename();
        path += ".dot";

        // Dump dot to file
        let file = std::path::Path::new(&path);
        std::fs::create_dir_all(file.parent().unwrap()).unwrap();
        let mut file = std::fs::File::create(file).unwrap();
        self.write_dot(&mut file).unwrap();
        Some(path)
    }

    fn filename(&self) -> String;
    fn write_dot(&self, f: &mut impl std::io::Write) -> std::io::Result<()>;
}

impl CanDot for DepGraph {
    fn filename(&self) -> String {
        "callgraph".to_string()
    }

    fn write_dot(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        use petgraph::dot::*;
        let gea = |_, _| "".to_string();
        let gna = |_, _| "".to_string();
        let dot = Dot::with_attr_getters(self, &[Config::EdgeNoLabel], &gea, &gna);
        write!(f, "{:?}", dot)
    }
}
