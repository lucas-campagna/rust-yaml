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

    let original = r#"key1:
  - item1
  - item2
key2:
  - item3
  - item4
"#;
    let parsed = yaml.load_str(original).unwrap();

    let mut map = IndexMap::new();
    let seq1 = vec![Value::String("item1".into()), Value::String("item2".into())];
    let seq2 = vec![Value::String("item3".into()), Value::String("item4".into())];
    map.insert(Value::String("key1".into()), Value::Sequence(seq1.clone()));
    map.insert(Value::String("key2".into()), Value::Sequence(seq2.clone()));
    let expected = Value::Mapping(map);
    let round_trip = yaml.dump_str(&expected).unwrap();
    assert_eq!(parsed, expected);
    assert_eq!(round_trip, original);
}

#[test]
fn test_mapping_sequence_formatting_complex() {
    let yaml = Yaml::new();

    let original = r#"card1:
  body:
    - h1: title
    - p: content
card2:
  from: div
  class: card
"#;
    let parsed = yaml.load_str(original).unwrap();

    let mut map_outter = IndexMap::new();
    let mut map_card1 = IndexMap::new();
    let mut map_card2 = IndexMap::new();
    let mut map_h1 = IndexMap::new();
    let mut map_p = IndexMap::new();
    map_h1.insert(Value::String("h1".into()), Value::String("title".into()));
    map_p.insert(Value::String("p".into()), Value::String("content".into()));
    let seq_body = vec![Value::Mapping(map_h1), Value::Mapping(map_p)];
    map_card1.insert(Value::String("body".into()), Value::Sequence(seq_body));
    map_card2.insert(Value::String("from".into()), Value::String("div".into()));
    map_card2.insert(Value::String("class".into()), Value::String("card".into()));
    map_outter.insert(Value::String("card1".into()), Value::Mapping(map_card1));
    map_outter.insert(Value::String("card2".into()), Value::Mapping(map_card2));

    let expected = Value::Mapping(map_outter);
    let round_trip = yaml.dump_str(&expected).unwrap();
    assert_eq!(parsed, expected);
    assert_eq!(round_trip, original);
}

#[test]
fn test_mapping_sequence_mapping() {
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