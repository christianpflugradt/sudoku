// release.config.cjs
module.exports = {
    branches: ["main"],
    tagFormat: "v${version}",
    plugins: [
        [
            "@semantic-release/commit-analyzer",
            {
                preset: "conventionalcommits",
                releaseRules: [
                    { type: "feat", release: "minor" },

                    { type: "fix", release: "patch" },
                    { type: "perf", release: "patch" },
                    { type: "chore", release: "patch" },

                    { type: "refactor", release: "patch" },
                    { type: "build", release: "patch" },
                    { type: "ci", release: "patch" },
                    { type: "docs", release: "patch" },
                    { type: "test", release: "patch" },
                ],
            },
        ],
        [
            "@semantic-release/release-notes-generator",
            {
                preset: "conventionalcommits",
                presetConfig: {
                    types: [
                        { type: "feat", section: "🚀 Features" },
                        { type: "fix", section: "🐛 Fixes" },
                        { type: "refactor", section: "♻️ Refactoring" },
                        { type: "perf", section: "⚡ Performance" },
                        { type: "test", section: "🧪 Tests" },
                        { type: "docs", section: "📚 Documentation" },
                        { type: "build", section: "🏗️ Build & Tooling" },
                        { type: "ci", section: "🤖 CI" },
                        { type: "chore", section: "🔧 Chores" },
                    ],
                },
            },
        ],
        "@semantic-release/github",
    ],
};
