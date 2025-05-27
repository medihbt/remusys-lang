use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod normalize;
pub mod typing;
pub mod util;

lalrpop_mod!(pub grammar);

#[cfg(test)]
mod tests {
    use crate::ast::print::AstPrinter;

    use super::*;
    use std::io::Write;

    #[test]
    fn parsing_test() {
        let base_path = "target/functional-sy";
        // let base_path = "target/self-test";
        let base_path = std::path::Path::new(base_path);

        for entry in std::fs::read_dir(base_path).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().is_none() || path.extension().unwrap() != "sy" {
                println!("Skipping non-SysY file: {:?}", path);
                continue; // Skip non-SysY files
            }
            let content = std::fs::read_to_string(&path).unwrap();
            println!("Parsing file: {:?}", path);
            std::io::stderr().flush().unwrap();
            let mut result = grammar::AstModuleParser::new()
                .parse(&content)
                .expect("Failed to parse `SysY` file");
            result.file = path.to_string_lossy().to_string();
            println!("Parsed file: {:?}", path);
            let writer_base_path = path.with_extension("ast");
            let writer = std::fs::File::create(writer_base_path).unwrap();
            let mut writer = std::io::BufWriter::new(writer);
            let mut priner = AstPrinter::new(&result, &mut writer);
            priner.print_module();
        }
    }
}
