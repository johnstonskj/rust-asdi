use std::error::Error;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "asdi", about = "Datalog.")]
struct CommandLine {
    /// The level of logging to perform; from off to trace
    #[structopt(long, short = "v", parse(from_occurrences))]
    verbose: i8,

    #[structopt(subcommand)]
    cmd: Command,
}

#[derive(Debug, StructOpt)]
enum Command {
    Check { file: PathBuf },
    Run { file: PathBuf, evaluator: Evaluator },
}

#[derive(Debug, Display)]
enum Evaluator {
    None,
    Naive,
    Stratified,
}

impl FromStr for Evaluator {}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CommandLine::from_args();
    Ok(())
}
