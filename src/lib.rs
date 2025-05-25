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

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
