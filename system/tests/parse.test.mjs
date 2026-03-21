// TODO: This test is incomplete / has never been run. 22.07.15.03.58
// import {jest} from '@jest/globals';
import { parse } from '../public/aesthetic.computer/lib/parse.mjs';

test('testing ~niki in parser', () => {
  expect(parse('~niki')).toEqual({
    path: "index",
    host: "niki.aesthetic.computer",
    params: [],
    search: "",
    hash: ""
  });
});