module [parse]

import parse.Parse as P exposing [lhs, rhs, both]

pound = P.char |> P.filter(|c| c == '#')
open_bracket = P.char |> P.filter(|c| c == '[')
close_bracket = P.char |> P.filter(|c| c == ']')
dot = P.char |> P.filter(|c| c == '.')
backslash = P.char |> P.filter(|c| c == '\\')
char_excluding_newline = P.char |> P.filter(|c| c != '\n')
char_excluding_bracket = P.char |> P.filter(|c| c != ']')
str_to_newline = P.one_or_more(char_excluding_newline) |> P.map (|bs| bs |> Str.from_utf8)
story_char = P.char |> P.filter(|c| c != '\\' and c != '[' and c != ']')

title = pound |> rhs(P.whitespace) |> rhs(str_to_newline)
expect title("# Title") |> P.finalize == Ok("Title")

blank = open_bracket |> rhs(P.integer) |> lhs(dot) |> lhs(P.whitespace) |> both(P.one_or_more(char_excluding_bracket) |> P.map(|bs| bs |> Str.from_utf8)) |> lhs(close_bracket) |> P.map(|(i, s)| Ok(Blank({ index: i, part_of_speech: s, answer: "" })))

expect blank("[20. Noun (plural)]") |> P.finalize == Ok(Blank({ index: 20, part_of_speech: "Noun (plural)", answer: "" }))

back_reference = open_bracket |> rhs("same as " |> P.string) |> rhs(P.maybe(pound)) |> rhs(P.integer) |> lhs(close_bracket) |> P.map(|i| Ok(BackReference({index: i, answer: ""})))
expect back_reference("[same as 20]") |> P.finalize == Ok(BackReference({index: 20, answer: ""}))

begin_story = backslash |> rhs(backslash) |> rhs("BEGIN STORY" |> P.string) |> rhs(P.whitespace) |> P.map(|_| Ok(BeginStory))
end_story = backslash |> rhs(backslash) |> rhs("END STORY" |> P.string) |> rhs(P.maybe(P.whitespace)) |> P.map(|_| Ok(EndStory))
story_text = P.one_or_more(story_char) |> P.map(|bs| bs |> Str.from_utf8_lossy |> Text |> Ok)
story = begin_story |> rhs(P.one_or_more(P.one_of([blank, back_reference, story_text]))) |> lhs(end_story)

expect story("\\\\BEGIN STORY\nNothing to see here.\\\\END STORY") |> P.finalize == Ok([Text("Nothing to see here.")])
expect story("\\\\BEGIN STORY\n[20. Noun (plural)]\\\\END STORY") |> P.finalize == Ok([Blank({ index: 20, part_of_speech: "Noun (plural)", answer: "" })])
expect story("\\\\BEGIN STORY\n[20. Noun (plural)][same as 20]\\\\END STORY") |> P.finalize == Ok([Blank({ index: 20, part_of_speech: "Noun (plural)", answer: "" }), BackReference({index: 20, answer: ""})])
expect story("\\\\BEGIN STORY\nOnce there was a [20. Noun], and what a [same as 20] it was. Good talk.\\\\END STORY") |> P.finalize == Ok([Text("Once there was a "), Blank({ index: 20, part_of_speech: "Noun", answer: "" }), Text(", and what a "), BackReference({index: 20, answer: ""}), Text(" it was. Good talk.")])

parse = |str|
    pattern = title |> lhs(P.zero_or_more(story_char)) |> both(story) 
    parser = pattern |> P.map(
        |(t, s)| 
            blanks = 
                List.walk(
                    s, 
                    0, 
                    |n, p| 
                        when p is
                            Blank({ index }) if index > n -> index
                            _ -> n
                )
            Ok({ title: t, story: s, blanks })
    )
    parser(str) |> P.finalize_lazy

expect 
    import "./The Mysterious Space Station/template.md" as text : Str
    expected = 
        Ok(
            {
                title: "The Mysterious Space Station",
                story: [
                    Text("One night, as I was staring at the "),
                    Blank({ index: 1, part_of_speech: "adjective", answer: "" }),
                    Text(" sky, I noticed something strange hovering over the "),
                    Blank({ index: 2, part_of_speech: "geographical location", answer: "" }),
                    Text(". It looked like (a/an) "),
                    Blank({ index: 3, part_of_speech: "noun", answer: "" }),
                    Text(", but it moved like (a/an) "),
                    Blank({ index: 4, part_of_speech: "animal", answer: "" }),
                    Text(". Curious, I grabbed my "),
                    Blank({ index: 5, part_of_speech: "noun", answer: "" }),
                    Text(", hopped on my bike, and followed it past the "),
                    Blank({ index: 6, part_of_speech: "generic place", answer: "" }),
                    Text(".\n\nTo my surprise, the object landed behind (a/an) "),
                    Blank({ index: 7, part_of_speech: "adjective", answer: "" }),
                    Text(" hill. As I approached, I saw what looked like (a/an) "),
                    Blank({ index: 8, part_of_speech: "noun", answer: "" }),
                    Text(" with blinking lights and a door that opened with a loud \""),
                    Blank({ index: 9, part_of_speech: "exclamation", answer: "" }),
                    Text("!\" I stepped inside and was greeted by (a/an) "),
                    Blank({ index: 10, part_of_speech: "occupation", answer: "" }),
                    Text(" wearing (a/an) "),
                    Blank({ index: 11, part_of_speech: "adjective", answer: "" }),
                    Text(" suit.\n\nThe inside was filled with "),
                    Blank({ index: 12, part_of_speech: "plural noun", answer: "" }),
                    Text(" floating in zero gravity. Suddenly, I slipped on (a/an) "),
                    Blank({ index: 13, part_of_speech: "noun", answer: "" }),
                    Text(" and crashed into a panel, accidentally activating the "),
                    BackReference({index: 3, answer: ""}),
                    Text("'s engines. The entire station started (a/an) "),
                    Blank({ index: 14, part_of_speech: "verb (ending in -ing)", answer: "" }),
                    Text(" through space!\n\nWe zoomed past "),
                    Blank({ index: 15, part_of_speech: "plural noun", answer: "" }),
                    Text(" and a planet shaped like (a/an) "),
                    Blank({ index: 16, part_of_speech: "silly word", answer: "" }),
                    Text(". The "),
                    BackReference({index: 10, answer: ""}),
                    Text(" shouted \""),
                    BackReference({index: 9, answer: ""}),
                    Text("!\" again and pressed a button with (a/an) "),
                    Blank({ index: 17, part_of_speech: "part of the body", answer: "" }),
                    Text(". We came to a stop near an alien market where they sold (a/an) "),
                    Blank({ index: 18, part_of_speech: "plural noun", answer: "" }),
                    Text(" made from (a/an) "),
                    Blank({ index: 19, part_of_speech: "type of liquid", answer: "" }),
                    Text(".\n\nAfter a quick "),
                    Blank({ index: 20, part_of_speech: "noun", answer: "" }),
                    Text(", the ship brought me home. I woke up in my bed, wondering if it had all been a dream... until I saw the "),
                    BackReference({index: 18, answer: ""}),
                    Text(" on my desk.\n")
                ],
                blanks: 20
            }
        )
    result = parse(text) 
    result == expected