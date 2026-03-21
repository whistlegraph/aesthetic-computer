#!/usr/bin/env node

import { connect } from './database.mjs';
import { config } from 'dotenv';

config({ path: '../.env' });

const database = await connect();
const paintings = database.db.collection('paintings');

const painting = await paintings.findOne({ slug: '2025.10.17.15.13.52.014' });

console.log(painting);

await database.disconnect();
