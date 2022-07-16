use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;

#[derive(Debug)]
pub struct TxTar {
    header: String,
    sections: HashMap<String, String>,
}

const BEGIN_MARKER: &str = "-- ";
const END_MARKER: &str = " --";

pub fn parse(f: &mut dyn Read) -> Result<TxTar, std::io::Error> {
    let mut buf = String::new();
    f.read_to_string(&mut buf)?;
    let pat = Regex::new("-- .* --").expect("infalible");
    let parts: Vec<_> = buf.split_inclusive(&pat).collect();

    let (header, mut current_name) = match parts[0].rsplit_once("\n") {
        Some((h, f)) => (h.to_string(), f),
        None => ("".to_string(), parts[0]),
    };

    let mut sections = HashMap::new();
    for part in parts[1..].iter() {
        let (content, next_name) = match part.rsplit_once("\n") {
            Some((h, f)) => (h.to_string(), f),
            None => ("".to_string(), *part),
        };
        sections.insert(
            current_name
                .trim_start_matches(BEGIN_MARKER)
                .trim_end_matches(END_MARKER)
                .trim()
                .to_string(),
            content.trim_start().to_string(),
        );
        current_name = next_name;
    }

    return Ok(TxTar { header, sections });
}

#[test]
fn test_export_007() {
    let mut f = File::open(Path::new(
        "../../cue-lang/cue/cue/testdata/export/007.txtar",
        // "../../cue-lang/cue/cue/testdata/export/008.txtar",
    ))
    .unwrap();
    let tar = parse(&mut f);
    println!("tar: {:#?}", tar);
}
