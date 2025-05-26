use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod normalize;
pub mod typing;
pub mod util;

lalrpop_mod!(pub grammar);

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    // #[test]
    // fn it_works() {
    //     let result = add(2, 2);
    //     assert_eq!(result, 4);
    // }

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

            // Some tests generate a deep AST, while it causes a stack overflow.
            // Remember these tests and skip them.
            match path.file_name().and_then(|s| s.to_str()) {
                Some("86_long_code2.sy") => {
                    println!("[Skipping deeeeeeeeep AST test that may cause overflow: {:?}]", path);
                    continue;
                }
                Some(_) => writeln!(writer, "{:#?}", result).expect("Failed to write AST to file"),
                _ => {}
            }
        }
    }
}
