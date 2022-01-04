use asdi::edb::{Constant, Predicate};
use asdi::idb::Variable;
use asdi::AttributeName;
use proptest::proptest;
use std::str::FromStr;

// ------------------------------------------------------------------------------------------------
// Automated Property Tests
// ------------------------------------------------------------------------------------------------

proptest! {
    #[test]
    fn predicate_from_str(s in r"\p{Ll}[\p{Ll}\p{Lu}\p{Nd}_]*") {
        println!("predicate_from_str: {:?}", s);
        assert!(Predicate::is_valid(&s));
        assert!(Predicate::from_str(&s).is_ok());
    }

    #[test]
    fn variable_from_str(s in r"_|\p{Lu}[\p{Ll}\p{Lu}\p{Nd}_]*") {
        println!("variable_from_str: {:?}", s);
        assert!(Variable::is_valid(&s));
        assert!(Variable::from_str(&s).is_ok());
    }

    #[test]
    fn constant_identifier_from_str(s in r"\p{Ll}[\p{Ll}\p{Lu}\p{Nd}_]*(:[\p{Ll}\p{Lu}][\p{Ll}\p{Lu}\p{Nd}_]*)?") {
        println!("constant_identifier_from_str: {:?}", s);
        assert!(Constant::is_identifier(&s));
    }
}
