import assert from "assert";
import { test } from "node:test";
import path from "path";
import {
  Member_Nested,
  StructureGroups,
  Type,
} from "@workerd/jsg/rtti.capnp.js";
import { Message } from "capnp-ts";
import ts from "typescript";
import { generateDefinitions } from "../../../src/generator";
import { printNodeList, printer } from "../../../src/print";
import { createMemoryProgram } from "../../../src/program";
import {
  compileOverridesDefines,
  createOverrideDefineTransformer,
} from "../../../src/transforms";

function printDefinitionsWithOverrides(root: StructureGroups): string {
  const nodes = generateDefinitions(root);
  const [sources, replacements] = compileOverridesDefines(root);
  const sourcePath = path.resolve(__dirname, "source.ts");
  const source = printNodeList(nodes);
  sources.set(sourcePath, source);
  const program = createMemoryProgram(sources);
  const sourceFile = program.getSourceFile(sourcePath);
  assert(sourceFile !== undefined);
  const result = ts.transform(sourceFile, [
    createOverrideDefineTransformer(program, replacements),
  ]);
  assert.strictEqual(result.transformed.length, 1);
  return printer.printFile(result.transformed[0]);
}

test("createOverrideDefineTransformer: applies type renames", () => {
  const root = new Message().initRoot(StructureGroups);
  const group = root.initGroups(1).get(0);
  const structures = group.initStructures(2);

  const thing = structures.get(0);
  thing.setName("Thing");
  thing.setFullyQualifiedName("workerd::api::Thing");
  thing.setTsOverride("RenamedThing");
  function referenceThing(type: Type | Member_Nested) {
    const structureType = type.initStructure();
    structureType.setName("Thing");
    structureType.setFullyQualifiedName("workerd::api::Thing");
  }

  // Create type root that references Thing in different ways to test renaming
  const root1 = structures.get(1);
  root1.setName("Root1");
  root1.setFullyQualifiedName("workerd::api::Root1");
  root1.setTsRoot(true);
  {
    const members = root1.initMembers(3);

    const prop = members.get(0).initProperty();
    prop.setName("prop");
    referenceThing(prop.initType());

    const method = members.get(1).initMethod();
    method.setName("method");
    referenceThing(method.initArgs(1).get(0));
    referenceThing(method.initReturnType());

    const nested = members.get(2).initNested();
    nested.setName("Thing"); // Should keep original name
    referenceThing(nested);
  }

  assert.strictEqual(
    printDefinitionsWithOverrides(root),
    `export declare abstract class RenamedThing {
}
export interface Root1 {
    prop: RenamedThing;
    method(param0: RenamedThing): RenamedThing;
    Thing: typeof RenamedThing;
}
`
  );
});

test("createOverrideDefineTransformer: applies property overrides", () => {
  // TODO: never typed
});

test("createOverrideDefineTransformer: applies method overrides", () => {
  // TODO: never return
  // TODO: overloads
});

test("createOverrideDefineTransformer: applies type parameter overrides", () => {
  // TODO
});

test("createOverrideDefineTransformer: applies heritage overrides", () => {
  // TODO
});

test("createOverrideDefineTransformer: applies full type replacements", () => {
  // TODO
});

test("createOverrideDefineTransformer: inserts extra defines", () => {
  // TODO
});
