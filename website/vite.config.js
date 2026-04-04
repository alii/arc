import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import tailwindcss from '@tailwindcss/vite';
import fs from 'node:fs';
import path from 'node:path';

const coopCoep = {
	'Cross-Origin-Opener-Policy': 'same-origin',
	'Cross-Origin-Embedder-Policy': 'require-corp',
};

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
			const files = fs.readdirSync(examplesDir)
				.filter(f => f.endsWith('.js'))
				.sort();
			const examples = files.map(f => ({
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

export default defineConfig({
	plugins: [tailwindcss(), react(), examplesPlugin()],
	server: { headers: coopCoep },
	preview: { headers: coopCoep },
});
