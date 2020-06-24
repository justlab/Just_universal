# Evaluate a class of constant expressions.
#
# Based on a Stack Overflow answer by Hadley Wickham:
# https://stackoverflow.com/a/18391779

const.env = new.env(parent = emptyenv())
for (s in c(
        "T", "F",
        "{", "(", "[", "[[",
        getGroupMembers("Math"),
        getGroupMembers("Arith"),
        getGroupMembers("Compare"),
        "c", "list",
        "paste", "paste0", "sprintf"))
    const.env[[s]] = get(s, "package:base")

#' @export
eval.const = function(e)
    eval(e, env = const.env)
