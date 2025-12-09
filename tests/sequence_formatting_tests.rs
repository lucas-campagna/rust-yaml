#![allow(clippy::needless_raw_string_hashes)]

use rust_yaml::{Value, Yaml};
use indexmap::IndexMap;

#[test]
fn test_sequence_formatting_issue() {
    let yaml = Yaml::new();

    // Create the problematic structure: a sequence containing mappings and scalars
    let mut mapping1 = IndexMap::new();
    mapping1.insert(
        Value::String("key1".to_string()),
        Value::String("value1".to_string()),
    );

    let mut mapping2 = IndexMap::new();
    mapping2.insert(
        Value::String("key2".to_string()),
        Value::String("value2".to_string()),
    );

    let sequence = Value::Sequence(vec![
        Value::Mapping(mapping1),
        Value::Mapping(mapping2),
        Value::String("value3".to_string()),
    ]);

    let mut test_map = IndexMap::new();
    test_map.insert(Value::String("comp".to_string()), sequence);
    let test_value = Value::Mapping(test_map);

    // Dump the value to see the current formatting
    let dumped = yaml.dump_str(&test_value).unwrap();
    
    // The expected correct output (what we want):
    let expected_output = r#"comp:
  - key1: value1
  - key2: value2
  - value3
"#;
    
    // After fixing the formatter, this should pass
    assert_eq!(dumped, expected_output, "Sequence formatting should be correct");
}



#[test]
fn test_simple_sequence_formatting() {
    let yaml = Yaml::new();

    // Test a simpler case: sequence of scalars
    let sequence = Value::Sequence(vec![
        Value::String("item1".to_string()),
        Value::String("item2".to_string()),
        Value::String("item3".to_string()),
    ]);

    let dumped = yaml.dump_str(&sequence).unwrap();
    println!("Simple sequence output:\n{}", dumped);
    
    // Simple sequences should work correctly
    let expected = r#"- item1
- item2
- item3"#;
    
    assert_eq!(dumped.trim(), expected);
}

#[test]
fn test_mapping_sequence_formatting() {
    let yaml = Yaml::new();

    // Test mapping containing a sequence of scalars
    let sequence = Value::Sequence(vec![
        Value::String("item1".to_string()),
        Value::String("item2".to_string()),
    ]);

    let mut map = IndexMap::new();
    map.insert(Value::String("list".to_string()), sequence);
    let test_value = Value::Mapping(map);

    let dumped = yaml.dump_str(&test_value).unwrap();
    println!("Mapping with sequence output:\n{}", dumped);
    
    // This should work correctly (no space after colon when value is sequence)
    let expected = r#"list:
  - item1
  - item2"#;
    
    assert_eq!(dumped.trim(), expected);
}