app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    parse: "https://github.com/imclerran/roc-tinyparse/releases/download/v0.3.3/kKiVNqjpbgYFhE-aFB7FfxNmkXQiIo2f_mGUwUlZ3O0.tar.br",
    ai: "https://github.com/imclerran/roc-ai/releases/download/v0.10.1/iIKfbjobbmHIC5lW5pIWKkdMqVHX4IEgpdOO7EReYUM.tar.br",
    dt: "https://github.com/imclerran/roc-isodate/releases/download/v0.7.4/bEDedspHQApTvZI2GJfgXpzUmYO_ATw5d6xE_w1qcME.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.8.0/RQlGWlkQEfxtkSYKl0nHNQaOFT0-Jh7NNFEX2IPXlec.tar.br",
    rtils: "https://github.com/imclerran/rtils/releases/download/v0.1.7/xGdIJGyOChqLXjqx99Iqunxz3XyEpBp5zGOdb3OVUhs.tar.br",
}

import "./prompt.md" as prompt : Str

import RocLib exposing [RocLib]

import ai.Client
import ai.Chat
import ansi.ANSI exposing [color]
import cli.Cmd
import cli.Stdout
import cli.Stdin
import cli.File
import cli.Path
import cli.Http
import cli.Env
import cli.Utc
import dt.DateTime
import dt.Now {
    now!: Utc.now!,
    now_to_nanos: Utc.to_nanos_since_epoch,
}

main! = |_args|
    main_loop!({})

main_loop! = |{}|
    when main_menu!({}) is
        Ok(Continue) -> main_loop!({})
        Ok(Exit) -> Ok({})
        Err(e) ->
            Err(Exit(1, Inspect.to_str(e)))

get_save_dir = |template_path|
    template_path |> Path.display |> Str.split_on("/") |> List.drop_last(1) |> Str.join_with("/")

get_madlib_choices! = |{}|
    cwd = Env.cwd!({})?
    Path.list_dir!(cwd)?
    |> List.walk_try!([], |acc, path| if Path.is_dir!(path)? then Ok(List.append(acc, path)) else Ok(acc))?
    |> List.walk_try!(
        [],
        |acc, path|
            file_path = Path.display(path) |> Str.concat("/template.md") |> Path.from_str
            exists =
                Path.is_file!(file_path)
                |> Result.on_err(|e| if e == PathErr(NotFound) then Ok(Bool.false) else Err(e))?
            if exists then
                title = Path.display(path) |> Str.split_on("/") |> List.last?
                index = List.len(acc) + 1
                List.append(acc, (index, title, file_path)) |> Ok
            else
                Ok(acc),
    )

main_menu! = |{}|
    menu_options = ["1) Generate a new story", "2) Play an existing story", "3) Exit"]
    List.for_each_try!(
        menu_options,
        |option| option |> color({ fg: Standard Magenta }) |> Stdout.line!(),
    )?
    "\nSelect an option by number: " |> color({ fg: Standard Cyan }) |> Stdout.line!?
    choice = Stdin.line!({})? |> Str.to_u64
    when choice is
        Ok(1) ->
            play_new_story!({})

        Ok(2) ->
            play_existing_story!({})

        Ok(3) ->
            "\nGoodbye!" |> color({ fg: Standard Magenta }) |> Stdout.line!?
            Ok(Exit)

        _ ->
            "\nInvalid choice. Please try again.\n" |> color({ fg: Standard Yellow }) |> Stdout.line!?
            main_menu!({})

play_new_story! = |{}|
    "\nPlease enter a theme for your story (or leave blank for a surprise): "
    |> color({ fg: Standard Cyan })
    |> Stdout.line!?
    theme = Stdin.line!({})? |> |s| if Str.is_empty(s) then "Surprise me!" else s |> Str.trim
    "Give me a moment to write you a story..."
    |> |s| if theme == "Surprise me!" then s else Str.with_prefix(s, "\n")
    |> color({ fg: Standard Yellow })
    |> Stdout.line!?
    (roclib, template_path) = generate_madlib!(theme)?
    "\nOkay, I've got it! Let's play..."
    |> color({ fg: Standard Cyan })
    |> Stdout.line!?
    play_roclib!(roclib, template_path)

play_existing_story! = |{}|
    madlib_choices = get_madlib_choices!({}) ? |_| GetMadLibChoicesError
    madlib_choice = my_madlibs_menu!(madlib_choices)?
    template_path = madlib_choice.2
    roclib = load_story!(template_path) ? |_| LoadStoryError
    play_roclib!(roclib, template_path)

play_roclib! = |roclib, template_path|
    finished_madlib = get_answers!(roclib)?
    are_you_ready!({})?
    save_dir = get_save_dir(template_path)
    filename = Now.date_time!({}) |> DateTime.to_iso_str
    save_story!(finished_madlib, save_dir, filename) ? |_| FileSaveError
    html_path = Str.concat(save_dir, "/${filename}.html")
    open_story_html!(html_path)?
    continue!({})

open_story_html! = |file_path|
    when Env.platform!({}) |> .os is
        LINUX -> Cmd.exec!("xdg-open", [file_path])
        MACOS -> Cmd.exec!("open", [file_path])
        _ -> Err(UnsupportedPlatform)

are_you_ready! = |{}|
    "\nARE YOU READY?!?!"
    |> color({ fg: Standard Red })
    |> Stdout.line!?
    (if Str.is_empty(Stdin.line!({})?) then "Opening your story..." else "\nOpening your story...")
    |> color({ fg: Standard Magenta })
    |> Stdout.line!

continue! = |{}|
    "\nPress enter to return to the main menu..."
    |> color({ fg: Standard Cyan })
    |> Stdout.line!?
    _ = Stdin.line!({})
    Ok(Continue)

my_madlibs_menu! = |choices|
    print_madlibs_menu!(choices)?
    "\nSelect a story by number: " |> color({ fg: Standard Cyan }) |> Stdout.line!?
    when get_madlibs_menu_choice!(choices) is
        Err(InvalidChoice(index)) ->
            "\nInvalid choice: ${index |> Num.to_str}. Please try again.\n"
            |> color({ fg: Standard Yellow })
            |> Stdout.line!?
            my_madlibs_menu!(choices)

        Ok(choice) -> Ok(choice)
        Err(e) -> Err(e)

print_madlibs_menu! = |choices|
    List.for_each_try!(
        choices,
        |(index, title, _file_path)|
            "${index |> Num.to_str}) ${title}"
            |> color({ fg: Standard Magenta })
            |> Stdout.line!,
    )

get_madlibs_menu_choice! = |choices|
    index = Stdin.line!({})? |> Str.to_u64?
    choices |> List.find_first(|(i, _, _)| i == index) |> Result.map_err(|_| InvalidChoice(index))

load_story! : Path.Path => Result RocLib _
load_story! = |path|
    file_path = Path.display(path)
    text = File.read_utf8!(file_path) |> Result.map_err(|_| FailedToReadFile(file_path))?
    RocLib.parse_template(text)

get_answers! = |roclib|
    story =
        roclib.story
        |> List.walk_try!(
            [],
            |acc, part|
                when part is
                    Blank({ index, part_of_speech }) ->
                        answer = get_answer!(part_of_speech, index, roclib.blanks)?
                        List.append(acc, Blank({ index, part_of_speech, answer })) |> Ok

                    BackReference({ index }) ->
                        answer = RocLib.lookup_reference(index, acc) |> Result.with_default("# reference error#")
                        List.append(acc, BackReference({ index, answer })) |> Ok

                    Text(text) ->
                        List.append(acc, Text(text)) |> Ok,
        )?
    Ok { roclib & story }

get_answer! = |part_of_speech, index, total|
    progress = index |> Num.to_str |> Str.concat("/") |> Str.concat(total |> Num.to_str)
    "\n(${progress}) Please enter a ${part_of_speech}: "
    |> color({ fg: Standard Cyan })
    |> Stdout.line!?
    answer = Stdin.line!({})? |> Str.trim
    if Str.is_empty(answer) then
        "Answer cannot be empty.\nPlease enter a ${part_of_speech}: "
        |> color({ fg: Standard Yellow })
        |> Stdout.line!?
        get_answer!(part_of_speech, index, total)
    else
        Ok(answer)

save_story! = |roclib, save_dir, filename|
    html_name = Str.concat(filename, ".html")
    html_path = Str.concat(save_dir, "/${html_name}") |> Path.from_str
    html = RocLib.to_html(roclib)
    Path.write_utf8!(html, html_path)

generate_madlib! = |theme|
    api_key = Env.var!("OPENROUTER_API_KEY") |> Result.with_default("")
    model = "openai/gpt-4o"
    api = OpenRouter
    message_text = "${prompt}\n\nUSER SELECTED THEME:\n${theme}"
    client =
        Client.new({ api, api_key, model })
        |> Chat.add_user_message(message_text, {})
    generate_madlib_help!(theme, 3, client)

generate_madlib_help! = |theme, tries, client|
    req = Chat.build_http_request(client, {})
    resp = Http.send!(req)?
    when resp.status is
        200 ->
            message = Chat.decode_top_message_choice(resp.body)?
            template = message.content |> Str.drop_prefix("```md") |> Str.drop_prefix("```") |> Str.drop_suffix("```") |> Str.trim
            when RocLib.parse_template(template) is
                Ok(roclib) ->
                    dirname = roclib.title |> no_punctuation
                    cwd = Env.cwd!({})? |> Path.display
                    dirpath = Str.concat(cwd, "/${dirname}")
                    Path.create_dir!(Path.from_str(dirpath))?
                    template_path = Str.concat(dirpath, "/template.md") |> Path.from_str
                    Path.write_utf8!(template, template_path)?
                    Ok((roclib, template_path))

                Err(_) if tries > 0 ->
                    retry_message = "There was a problem parsing your story, please try again and pay careful attention"
                    new_client =
                        client
                        |> Chat.update_messages(resp)?
                        |> Chat.add_user_message(retry_message, {})
                    generate_madlib_help!(theme, tries - 1, new_client)

                Err(_) -> Err(MadLibParseError)

        _ ->
            Err(HttpStatusError(resp.status))

no_punctuation = |str|
    is_alpha = |c| (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c == ' ')
    str
    |> Str.to_utf8
    |> List.keep_if(is_alpha)
    |> Str.from_utf8_lossy
