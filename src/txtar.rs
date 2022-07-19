use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;

#[derive(Debug)]
pub struct TxTar<'a> {
    header: &'a str,
    sections: HashMap<&'a str, &'a str>,
}

impl<'a> TxTar<'a> {
    pub fn header(&self) -> &str {
        &self.header
    }
    pub fn get_section(&self, key: &'a str) -> Option<&'a str> {
        self.sections.get(key).map(|s| *s)
    }
}

const BEGIN_MARKER: &str = "-- ";
const END_MARKER: &str = " --";

pub fn parse<'a>(buf: &'a str) -> TxTar<'a> {
    let pat = Regex::new("-- .* --").expect("infalible");
    let parts: Vec<_> = buf.split_inclusive(&pat).collect();

    let (header, mut current_name) = match parts[0].rsplit_once("\n") {
        Some((h, f)) => (h, f),
        None => ("", parts[0]),
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
                .trim(),
            content.trim_start(),
        );
        current_name = next_name;
    }

    return TxTar { header, sections };
}

#[test]
fn test_export_007() {
    let mut f = File::open(Path::new(
        "../../cue-lang/cue/cue/testdata/export/007.txtar",
        // "../../cue-lang/cue/cue/testdata/export/008.txtar",
    ))
    .unwrap();
    let mut buf = String::new();
    f.read_to_string(&mut buf).expect("file exists");
    let tar = parse(buf.as_str());
    println!("tar: {:#?}", tar);
}
