import {format} from "kdljs";

export const unsafeFormatKdl = nodes => format(nodes, { indentChar: "\t", indent: 1 });

export const null_ = null
