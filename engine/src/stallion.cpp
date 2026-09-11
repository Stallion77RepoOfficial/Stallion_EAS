 
#include "search.h"
#include "uci.h"
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace {

std::vector<std::string_view> collect_args(int argc, char* argv[]) {
    return std::vector<std::string_view>(argv, argv + argc);
}

// Any invocation with CLI args other than "uci" runs the given command(s)
// once and then exits, instead of entering the interactive UCI loop. E.g.
// "./stallion perft 5" or "./stallion bench" run and terminate; "./stallion"
// (no args) or "./stallion uci" enter interactive UCI mode over stdin.
std::optional<int> handle_cli_mode(const std::vector<std::string_view>& args,
                                   BoardState& position,
                                   ThreadInfo& thread_info) {
    if (args.size() <= 1 || args[1] == "uci") {
        return std::nullopt;
    }

    std::string cmdline;
    for (size_t i = 1; i < args.size(); ++i) {
        if (i > 1) cmdline += ' ';
        cmdline += std::string(args[i]);
    }

    std::istringstream cli_stream(cmdline);
    uci(thread_info, position, cli_stream, /*interactive=*/false);
    return 0;
}

}

int main(int argc, char* argv[]) {
    BoardState position;
    auto thread_info = std::make_unique<ThreadInfo>();
    init_LMR();
    init_bbs();
    resize_TT(256);

    const auto args = collect_args(argc, argv);
    if (const auto exit_code = handle_cli_mode(args, position, *thread_info)) {
        return *exit_code;
    }

    uci(*thread_info, position);
    return 0;
}
