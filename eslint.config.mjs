import globals from "globals";
import path from "node:path";
import { fileURLToPath } from "node:url";
import js from "@eslint/js";
import { FlatCompat } from "@eslint/eslintrc";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const compat = new FlatCompat({
    baseDirectory: __dirname,
    recommendedConfig: js.configs.recommended,
    allConfig: js.configs.all
});

export default [...compat.extends("eslint:recommended"), {
    languageOptions: {
        globals: {
            ...globals.browser,
        },

        ecmaVersion: 6,
        sourceType: "module",
    },

    rules: {
        "block-scoped-var": "error",
        "consistent-return": "error",
        eqeqeq: "error",
        "guard-for-in": "error",
        "no-bitwise": "error",
        "no-caller": "error",
        "no-extra-parens": "off",
        "no-extend-native": "error",
        "no-loop-func": "error",
        "no-new": "error",
        "no-param-reassign": "error",
        "no-return-assign": "error",
        "no-sequences": "error",
        "no-unused-expressions": "error",
        "no-use-before-define": "error",
        "no-undef": "error",
        "no-eq-null": "error",
        radix: ["error", "always"],

        indent: ["error", 2, {
            SwitchCase: 1,
        }],

        quotes: ["error", "double"],
        semi: ["error", "always"],
        strict: ["error", "global"],
    },
}];