#include <algorithm>
#include <array>
#include <cstdint>
#include <expected>
#include <print>
#include <ranges>
#include <string_view>
#include <string>

enum class Mode { encode, decode };

namespace base64 {

constexpr std::string_view chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789+/";

constexpr auto make_table() {
    std::array<std::int8_t, 256> t{};
    t.fill(-1);

    std::size_t i = 0;
    for (unsigned char c : chars)
        t[c] = static_cast<std::int8_t>(i++);

    return t;
}

constexpr auto table = make_table();

constexpr bool is_b64(unsigned char c) {
    return table[c] != -1;
}

constexpr bool is_pad(char c) {
    return c == '=';
}

std::expected<Mode, std::string_view>
parse_mode(std::string_view m) noexcept {
    if (m == "encode") return Mode::encode;
    if (m == "decode") return Mode::decode;
    return std::unexpected("invalid mode");
}

bool valid(std::string_view s) {
    using std::ranges::all_of;

    if (s.empty() || s.size() % 4 != 0)
        return false;

    const auto pad_start = s.find('=');

    // no padding
    if (pad_start == std::string_view::npos) {
        return all_of(s, is_b64);
    }

    // padding must be at most 2 chars
    const auto pad_len = s.size() - pad_start;
    if (pad_len > 2)
        return false;

    // padding must be only at end
    if (!all_of(s | std::views::drop(pad_start), is_pad))
        return false;

    // prefix must be valid base64 alphabet
    return all_of(s | std::views::take(pad_start), is_b64);
}

} // namespace base64

std::string encode(std::string_view input) {
    std::string out;
    out.reserve(((input.size() + 2) / 3) * 4);

    std::uint32_t buf = 0;
    int bits = 0;

    for (unsigned char c : input) {
        buf = (buf << 8) | c;
        bits += 8;

        while (bits >= 6) {
            bits -= 6;
            out.push_back(base64::chars[(buf >> bits) & 0x3F]);
        }
    }

    if (bits > 0)
        out.push_back(base64::chars[(buf << (6 - bits)) & 0x3F]);

    while (out.size() % 4)
        out.push_back('=');

    return out;
}

std::expected<std::string, std::string_view>
decode(std::string_view input) {
    if (!base64::valid(input))
        return std::unexpected("invalid base64");

    std::string out;
    out.reserve((input.size() / 4) * 3);

    std::uint32_t buf = 0;
    int bits = 0;

    auto filtered = input | std::views::filter([](char c) {
        return c != '=';
    });

    for (unsigned char c : filtered) {
        const int v = base64::table[c];
        if (v < 0)
            return std::unexpected("invalid character");

        buf = (buf << 6) | static_cast<std::uint32_t>(v);
        bits += 6;

        while (bits >= 8) {
            bits -= 8;
            out.push_back(static_cast<char>((buf >> bits) & 0xFF));
        }
    }

    return out;
}

[[noreturn]] void usage() {
    std::println(stderr,
        "Usage: please provide a mode and a string to encode/decode");
    std::exit(1);
}

int main(int argc, char** argv) {
    if (argc != 3)
        usage();

    auto mode = base64::parse_mode(argv[1]);
    if (!mode)
        usage();

    std::string_view text = argv[2];
    if (text.empty())
        usage();

    switch (*mode) {
        case Mode::encode:
            std::println("{}", encode(text));
            break;

        case Mode::decode:
            if (auto res = decode(text))
                std::println("{}", *res);
            else
                usage();
            break;
    }
}