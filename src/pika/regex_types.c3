module pika;

struct CompiledRegex {
    Grammar grammar;
    int start_clause;
    int active_refs;
    bool anchored_start;
    bool anchored_end;
    bool cached;
    bool valid;
    char[256] error;
}
struct RegexMatch {
    bool matched;
    int start;
    int end;
    char[] view;
}

fn RegexMatch no_match() {
    RegexMatch m;
    m.matched = false;
    m.start = 0;
    m.end = 0;
    m.view = "";
    return m;
}

fn RegexMatch regex_mk_match(int start, int end, char[] input) {
    RegexMatch m;
    m.matched = true;
    m.start = start;
    m.end = end;
    m.view = "";  // Initialize to empty
    // C3 slices [a..b] are INCLUSIVE on both ends, giving (b-a+1) elements
    // So [start..end] already includes index 'end'
    if (end >= start && start >= 0 && end < (int)input.len) {
        m.view = input[start..end];
    }
    return m;
}
