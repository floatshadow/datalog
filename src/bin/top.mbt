///|
struct Options {
  mut file : String?
  mut output_to_console : Bool
  mut redirect_to_log : Bool
}

///|
fn Options::new() -> Options {
  Options::{ file: None, output_to_console: false, redirect_to_log: true }
}

///|
let opts : Options = Options::new()

///|
let tracer : @util.Tracer = @util.tracer

///|
fn main {
  let argv = @sys.get_cli_args()
  // *.wasm [args...]
  let truncated_argv = argv.split_at(1).1
  @ArgParser.parse(
    [
      (
        "--no-output-to-console",
        "-notc",
        @ArgParser.Unit(fn() { opts.output_to_console = false }),
        "",
      ),
      (
        "--output-to-console",
        "-otc",
        @ArgParser.Unit(fn() { opts.output_to_console = true }),
        "",
      ),
      (
        "--no-redirect-to-log",
        "-nrtl",
        @ArgParser.Unit(fn() { opts.redirect_to_log = false }),
        "",
      ),
      (
        "--redirect-to-log",
        "-rtl",
        @ArgParser.Unit(fn() { opts.redirect_to_log = true }),
        "",
      ),
    ],
    fn(s) {
      if opts.file.is_empty() {
        opts.file = Some(s)
      } else {
        abort("multiple files given")
      }
    },
    "",
    truncated_argv,
  )
  let file = match opts.file {
    Some(f) => f
    None => abort("no input file given")
  }
  try {
    // read file
    let contents = @fs.read_file_to_string(file)
    // lexer
    let lexer = @lexer.Lexer::new(contents)
    let tokens = []
    while true {
      match lexer.next_err() {
        (@parser.Token::Eof as eof, { start, end }) => {
          tokens.push((eof, start, end))
          break
        }
        (tok, { start, end }) => tokens.push((tok, start, end))
      }
    }
    // parser
    let subtracer = tracer.new_subtracer(
      "parser",
      opts.output_to_console,
      opts.redirect_to_log,
    )
    let stmts = @parser.parse_file(tokens)
    subtracer.log("parsed \{file} successfully")
    // evaluation
    let subtracer = tracer.new_subtracer(
      "evaluator",
      opts.output_to_console,
      opts.redirect_to_log,
    )
    let evaluator = @ast_eval.NaiveInterpreter::new(subtracer)
    let results = evaluator.evaluate(stmts)
    for result in results {
      println("\{Show::to_string(result)}")
    }
  } catch {
    Failure(msg) => println("\{msg}")
    _ as err => println("\{err}")
  }
  // logging
  try {
    let file_name = opts.file.unwrap().split(".").collect()
    let file_name = file_name[0:file_name.length() - 1]
    let file_name = file_name.map(fn(s) { s.to_string() }).join(".")
    let log_buf = @buffer.new()
    tracer.done(
      fn(_name, log) {
        log_buf.write_string(log)
        log_buf.write_string("\n")
      },
      fn(name, record) {
        @fs.write_string_to_file("\{file_name}.\{name}.txt", record)
      },
    )
    @fs.write_string_to_file("\{file_name}.log", log_buf.to_string())
  } catch {
    _ as err => println("write logging error, \{err}")
  }
}
