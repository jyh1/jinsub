Usage: jinsub ([COMMAND] | [-i|--interactive] [-t|--template TEMPLATE_NAME]
              [-f|--file TEMPLATE_FILE] [-v VAR=VALUE] [CMD] [--dry-run]
              [-s|--save-as FILEPATH])
  Quickly submit a pbs job

Available options:
  -i,--interactive         Whether display stdout in real time
  -t,--template TEMPLATE_NAME
                           PBS template name to use (default: "default")
  -f,--file TEMPLATE_FILE  Specify a template file path
  -v VAR=VALUE             Environment variables to set in template defination
                           placeholder
  CMD                      Command to run in pbs job
  --dry-run                Print generated pbs job file to stdout without
                           submitting to nodes
  -s,--save-as FILEPATH    Save generated pbs file to FILEPATH
  -h,--help                Show this help text

Available commands:
  edit                     Edit a template file
