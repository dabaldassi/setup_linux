
alias la='ls -A'
alias l='ls -CF'
alias ll="ls -l"

alias rmtmp="rm -f *~* *#*"

alias hugo-shell="docker run --rm -it -v $(pwd):/src klakegg/hugo:0.74.3-alpine shell"
alias hugo-server="docker run --rm -it -v $(pwd):/src -p 1313:1313 klakegg/hugo:0.74.3-ext server"

alias shrug="echo \"¯\_(ツ)_/¯\""

alias please=sudo

alias crun='cargo run'
alias cbuild='cargo build'
alias ccheck='cargo check'

alias emnoinit='emacs --no-init'

alias frame_number='ffprobe -v error -select_streams v:0 -count_frames -show_entries stream=nb_read_frames -of csv=p=0'

alias set_tc="sudo tc qdisc add dev lo root handle 1: netem delay 1ms && sudo tc qdisc add dev lo parent 1: handle 2: tbf rate 1mbit burst 20kb latency 400ms"
alias remove_tc="sudo tc qdisc delete dev lo root handle 1:"