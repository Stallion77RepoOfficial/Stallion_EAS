// Summary: Simplified CLI argument handling and factored mode dispatch for clarity.
#include "search.h"
#include "uci.h"
#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace {

std::vector<std::string_view> collect_args(int argc, char* argv[]) {
    std::vector<std::string_view> args;
    if (argc > 0) {
        args.reserve(static_cast<size_t>(argc));
    }
    for (int i = 0; i < argc; ++i) {
        args.emplace_back(argv[i]);
    }
    return args;
}

std::optional<int> handle_cli_mode(const std::vector<std::string_view>& args,
                                   Position& position,
                                   ThreadInfo& thread_info) {
    if (args.size() <= 1) {
        return std::nullopt;
    }

    const std::string_view mode = args[1];
    if (mode == "bench") {
        bench(position, thread_info);
        return 0;
    }

    if (mode == "perft") {
        if (args.size() <= 2) {
        safe_print_cerr(std::string("Error: perft mode requires a depth argument"));
            return 1;
        }

        set_board(position, thread_info,
                  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        try {
            const int depth = std::stoi(std::string(args[2]));
            auto start_time = std::chrono::steady_clock::now();
            uint64_t nodes = perft(depth, position, true, thread_info);
            auto elapsed_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::steady_clock::now() - start_time).count();
            uint64_t nps = (elapsed_ms > 0) ? (nodes * 1000 / elapsed_ms) : 0;
            safe_printf("%" PRIu64 " nodes %" PRIu64 " nps\n", nodes, nps);
            return 0;
        } catch (const std::exception&) {
            safe_print_cerr(std::string("Error: invalid depth '") + std::string(args[2]) + "' for perft");
            return 1;
        }
    }

    if (mode == "debug") {
    safe_printf("[DEBUG MODE] Not implemented yet.\n");
        return 0;
    }

    return std::nullopt;
}

}  // namespace

int main(int argc, char* argv[]) {
    Position position;
    auto thread_info = std::make_unique<ThreadInfo>();
    init_LMR();
    init_bbs();
    // Ensure transposition table matches UCI default Hash=256 on startup
    resize_TT(256);

    const auto args = collect_args(argc, argv);
    if (const auto exit_code = handle_cli_mode(args, position, *thread_info)) {
        return *exit_code;
    }

    uci(*thread_info, position);
    return 0;
}
