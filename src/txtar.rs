use regex::Regex;
use std::collections::HashMap;
use std::io::Result;

#[derive(Debug)]
pub struct TxTar {
    header: String,
    sections: HashMap<String, String>,
}

impl TxTar {
    pub fn header(&self) -> &String {
        &self.header
    }
    pub fn get_section(&self, key: &str) -> Option<&String> {
        self.sections.get(key).map(|s| s)
    }
    pub fn sections(&self) -> &HashMap<String, String> {
        &self.sections
    }
}

const BEGIN_MARKER: &str = "-- ";
const END_MARKER: &str = " --";

pub fn parse<'a>(buf: String) -> TxTar {
    let pat = Regex::new("-- .* --").expect("infalible");
    let parts: Vec<_> = buf.split_inclusive(&pat).collect();

    let (header, mut current_name) = match parts[0].rsplit_once("\n") {
        Some((h, f)) => (h.to_string(), f.to_string()),
        None => ("".to_string(), parts[0].to_string()),
    };

    let mut sections = HashMap::new();
    for part in parts[1..].iter() {
        let (content, next_name) = match part.rsplit_once("\n") {
            Some((h, f)) => (h, f),
            None => ("", *part),
        };
        sections.insert(
            current_name
                .trim_start_matches(BEGIN_MARKER)
                .trim_end_matches(END_MARKER)
                .trim()
                .to_string(),
            content.trim_start().to_string(),
        );
        current_name = next_name.to_string();
    }

    return TxTar { header, sections };
}

// pub fn parse_file<'a>(filename: &str) -> Result<(&str, TxTar)> {
pub fn parse_file<'a>(filename: &str) -> Result<TxTar> {
    let buf = std::fs::read_to_string(filename)?;

    let parsed = parse(buf);

    Ok(parsed)
}

#[test]
fn test_export_007() {
    let buf = std::fs::read_to_string(
        "../../cue-lang/cue/cue/testdata/export/007.txtar",
        // "../../cue-lang/cue/cue/testdata/export/008.txtar",
    )
    .unwrap();
    let tar = parse(buf);
    println!("tar: {:#?}", tar);
}
