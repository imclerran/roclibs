module [StoryPart, RocLib, parse_template, lookup_reference, to_html, to_markdown, to_str, story_to_str]

import Parser

Blank : { index : U64, part_of_speech: Str, answer: Str }
BackReference : { index: U64, answer: Str }
StoryPart : [Blank Blank, BackReference BackReference, Text(Str)]
RocLib : { title: Str, story: List StoryPart, blanks: U64 }

parse_template = Parser.parse

lookup_reference : U64, List StoryPart -> Result Str _
lookup_reference = |i, parts|
    parts
    |> List.find_first(
        |part|
            when part is
                Blank({ index }) if index == i -> Bool.true
                _ -> Bool.false
    )
    |> Result.map_ok(
        |part|
            when part is
                Blank({ answer }) -> answer
                _ -> crash "Will always be a Blank"
    )
    |> Result.map_err(|_| InvalidIndex(i))

story_to_str = |parts|
    parts 
    |> List.walk(
        "",
        |acc, part|
            when part is
                Blank({ answer }) -> Str.concat(acc, answer)
                BackReference({ answer }) -> Str.concat(acc, answer)
                Text(text) -> Str.concat(acc, text)
    )

to_str = |roclib|
    text = roclib.story |> story_to_str

    "${roclib.title}\n\n${text}"

to_markdown = |roclib|
    text = roclib.story |> story_to_str
    title = "# ${roclib.title}\n\n"

    "${title}${text}"

to_html = |roclib|
    paragraphs = 
        roclib.story 
        |> story_to_str 
        |> Str.split_on("\n") 
        |> List.drop_if(|s| Str.is_empty(s))
        |> List.map(
            |p| "<p>${p}</p>",
        )
        |> Str.join_with("")
    title = "<h1>${roclib.title}</h1>"
    style = "<style>body { background-color: #1e1e2e; margin: 2rem auto; max-width: 1000px; padding: 0 2rem; font-family: Calibri, 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; font-size: 170%; } h1 { color: #cba6f7; font-size: 170%; } p { color: #cdd6f4; }</style>"

    "<html><head>${style}</head><body>${title}${paragraphs}</body></html>"

to_html2 = |roclib|
    story_text = 
        roclib.story 
        |> List.walk(
				    "",
		        |acc, part|
		            when part is
		                BackReference({ answer }) -> Str.concat(acc, answer)
		                Text(text) -> Str.concat(acc, text)
										Blank({ answer }) -> 
												strong_answer = "<strong>${answer}</strong>"
												Str.concat(acc, strong_answer)
		    )
		paragraphs =
        story_text
        |> Str.split_on("\n") 
        |> List.drop_if(|s| Str.is_empty(s))
        |> List.map(|p| "<p>${p}</p>")
        |> Str.join_with("")
    title = "<h1>${roclib.title}</h1>"
    style = "<style>body { background-color: #1e1e2e; margin: 2rem auto; max-width: 1000px; padding: 0 2rem; font-family: Calibri, 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; font-size: 170%; } h1 { color: #cba6f7; font-size: 170%; } p { color: #cdd6f4; }</style>"

    "<html><head>${style}</head><body>${title}${paragraphs}</body></html>"

