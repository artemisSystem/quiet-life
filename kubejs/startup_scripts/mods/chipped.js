// priority: 5

// You could say I'm "chipping" a lot of this mod's content

// Utility functions for Chipped

// Which variants to keep, and how many variants exist in total, as well as
// which prefixes have items added by chipped (e.g. `oak_planks`), and which
// prefixes need items/blocks created (e.g. `livingwood_planks`)
// type ChippedVariant =
	// { keep :: Array Int
	// , total :: Int
	// , remove :: Array Int
	// , existingPrefixes :: Array ResourceLocation
	// , additionalPrefixes :: Array ResourceLocation
	// , properties :: Properties
	// }
const chippedVariant = (keep, total, existingPrefixes, additionalPrefixes, properties) => ({
	keep: keep, total: total, remove: difference(numbers(total))(keep),
	existingPrefixes: existingPrefixes.map(rl("chipped")),
	additionalPrefixes: additionalPrefixes.map(rl("kubejs")),
	properties: properties
});

// Data

const chippedStoneTypes = [
	"granite", "diorite", "andesite", "prismarine", "dark_prismarine",
	"purpur_block", "quartz_block", "sandstone", "red_sandstone", "nether_bricks",
	"red_nether_bricks", "stone", "cobblestone", "end_stone", "netherrack"
].concat(colorVariants(["terracotta", "concrete"]));

const chippedVariants = {
	planks: chippedVariant([1, 2, 3, 4, 5], 18,
		global.vanillaWoodTypes.map(x => `${x.id}_planks`),
		global.moddedWoodTypes.map(x => `${x.id}_planks`)
	),
	stone: chippedVariant([], 18, chippedStoneTypes, []),
	blackstone: chippedVariant([], 21, ["blackstone"], []),
	basalt: chippedVariant([], 20, ["basalt"], []),
	wool: chippedVariant([], 18, colorVariants(["wool", "carpet"]), [])
};

// Additions and removals

onEvent("block.registry", event => {
	const additions = reduceObj((acc, variant) =>
		acc.concat
	)([])(chippedVariants);
});

global.queueRemovals(reduceObj((acc, variant) =>
	acc.concat(lift2(prefix => remove => `${rlStr(prefix)}_${remove}`)
		(variant.existingPrefixes)(variant.remove))
)([])(chippedVariants));