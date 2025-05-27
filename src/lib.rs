use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod normalize;
pub mod parser;
pub mod typing;
pub mod util;

lalrpop_mod!(pub grammar);

#[cfg(test)]
mod tests {
    use crate::ast::print::AstPrinter;

    use super::*;
    use std::{
        io::Write,
        thread::{self},
    };

    #[test]
    fn parsing_test() {
        let base_path = "target/functional-sy";
        // let base_path = "target/self-test";
        let base_path = std::path::Path::new(base_path);

        let mut nfiles = 0;
        for entry in std::fs::read_dir(base_path).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().is_none() || path.extension().unwrap() != "sy" {
                eprintln!("Skipping non-SysY file: {:?}", path);
                continue; // Skip non-SysY files
            }
            eprintln!("============== Parsing file: {:?} ================", path);
            std::io::stdout().flush().unwrap();
            std::io::stderr().flush().unwrap();

            let thrd = thread::Builder::new()
                .name(format!("Parser-{}", nfiles))
                .stack_size(64 * 1024 * 1024) 
                .spawn(move || {
                    // Simulate some work
                    let module = parser::parse_sysy_file(path.to_str().unwrap());
                    let module = normalize::AstNormalizer::new(&module).normalize();
                    eprintln!("=============== Parsed file: {:?} ================", path);
                    let writer_base_path = path.with_extension("ast");
                    let writer = std::fs::File::create(writer_base_path).unwrap();
                    let mut writer = std::io::BufWriter::new(writer);
                    let mut priner = AstPrinter::new(&module, &mut writer);
                    priner.print_module();
                });
            thrd.unwrap().join().unwrap();
            nfiles += 1;
        }
        eprintln!("Parsed {} files successfully.", nfiles + 1);
    }
}
