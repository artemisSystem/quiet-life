import sharp from "sharp";

export const readRawTextureData = file => () => sharp(file)
		.raw()
		.toBuffer({ resolveWithObject: true })
		.then(({data, info: { width, height, channels }}) => ({
			data, width, height, channels
		}))

export const writeRawTextureData = file => ({ width, height, buffer }) => () =>
	sharp(buffer, { raw: { width, height, channels: 4 } })
		.png()
		.toFile(file);
