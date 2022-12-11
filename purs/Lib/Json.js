export const unsafeFormatJson = json => JSON.stringify(json, null, "\t");

export const parseJson = str => () => JSON.parse(str);