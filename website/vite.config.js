// @ts-check

import tailwindcss from '@tailwindcss/vite';
import react from '@vitejs/plugin-react';
import fs from 'node:fs';
import path from 'node:path';
import { defineConfig } from 'vite';

const coopCoep = {
	'Cross-Origin-Opener-Policy': 'same-origin',
	'Cross-Origin-Embedder-Policy': 'require-corp',
};

/** @returns {import("vite").PluginOption} */
function examplesPlugin() {
	const virtualId = 'virtual:examples';
	const resolved = '\0' + virtualId;
	const examplesDir = path.resolve(__dirname, '../examples');

	return {
		name: 'arc-examples',
		resolveId(id) {
			if (id === virtualId) return resolved;
		},
		load(id) {
			if (id !== resolved) return;
			const files = fs
				.readdirSync(examplesDir)
				.filter((f) => f.endsWith('.js'))
				.sort();
			const examples = files.map((f) => ({
				name: f.replace(/\.js$/, '').replace(/[_-]/g, ' '),
				code: fs.readFileSync(path.join(examplesDir, f), 'utf-8').trim(),
			}));
			return `export default ${JSON.stringify(examples)};`;
		},
		handleHotUpdate({ file, server }) {
			if (file.startsWith(examplesDir)) {
				const mod = server.moduleGraph.getModuleById(resolved);
				if (mod) return [mod];
			}
		},
	};
}

/** @returns {import("vite").PluginOption} */
function test262HistoryPlugin() {
	const virtualId = 'virtual:test262-history';
	const resolved = '\0' + virtualId;
	const file = path.resolve(__dirname, '../.github/test262/history.json');

	return {
		name: 'arc-test262-history',
		resolveId(id) {
			if (id === virtualId) return resolved;
		},
		load(id) {
			if (id !== resolved) return;
			return `export default ${fs.readFileSync(file, 'utf-8')};`;
		},
		handleHotUpdate({ file: changed, server }) {
			if (changed === file) {
				const mod = server.moduleGraph.getModuleById(resolved);
				if (mod) return [mod];
			}
		},
	};
}

export default defineConfig({
	plugins: [tailwindcss(), react(), examplesPlugin(), test262HistoryPlugin()],
	server: { headers: coopCoep },
	preview: { headers: coopCoep },
});
