pub fn indent(mut s: String, level: usize) -> String {
    for _ in 0..level {
        s.insert_str(0, "    ");
    }
    s
}

