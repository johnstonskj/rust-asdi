use asdi::edb::Constant;

#[test]
fn test_is_identifier() {
    assert!(Constant::is_identifier("socrates"));
    assert!(Constant::is_identifier("socrates_and_plato"));
    assert!(Constant::is_identifier("socrates1"));
    assert!(Constant::is_identifier("greek:socrates"));
    assert!(Constant::is_identifier("greek:Socrates"));
    assert!(Constant::is_identifier("greek:socrates_and_plato"));
    assert!(Constant::is_identifier("greek:Socrates_and_Plato"));
    assert!(Constant::is_identifier("greek:socrates1"));
    assert!(Constant::is_identifier("greek:Socrates1"));
}

#[test]
fn test_is_not_identifier() {
    assert!(!Constant::is_identifier("Socrates"));
    assert!(!Constant::is_identifier("_and_plato"));
    assert!(!Constant::is_identifier("1socrates"));
    assert!(!Constant::is_identifier("Greek:socrates"));
    assert!(!Constant::is_identifier("_greek:socrates"));
    assert!(!Constant::is_identifier("greek:_and_plato"));
    assert!(!Constant::is_identifier("greek:socrates:plato"));
    assert!(!Constant::is_identifier(":greekSocrates"));
    assert!(!Constant::is_identifier("greek:"));
}
