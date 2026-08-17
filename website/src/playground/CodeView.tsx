import { StreamLanguage } from '@codemirror/language';
import { erlang } from '@codemirror/legacy-modes/mode/erlang';
import { Compartment, EditorState } from '@codemirror/state';
import { EditorView } from '@codemirror/view';
import { useEffect, useRef } from 'react';
import { baseTheme, getIsDark, themeExtensions, watchColorScheme } from './theme';

export type CodeLanguage = 'erlang' | 'plain';

const erlangLanguage = StreamLanguage.define(erlang);

/**
 * Read-only CodeMirror viewer for compiler output. Mounted once; `code`
 * changes are applied as document replacements so a 100KB Erlang dump doesn't
 * re-create the editor (and keeps the scroll position when only whitespace
 * shifts).
 */
export function CodeView({ code, language, className }: { code: string; language: CodeLanguage; className?: string }) {
	const hostRef = useRef<HTMLDivElement>(null);
	const viewRef = useRef<EditorView | null>(null);
	const languageRef = useRef(new Compartment());

	useEffect(() => {
		if (!hostRef.current) return;
		const themeCompartment = new Compartment();
		const view = new EditorView({
			state: EditorState.create({
				doc: code,
				extensions: [
					EditorState.readOnly.of(true),
					EditorView.editable.of(false),
					baseTheme,
					themeCompartment.of(themeExtensions(getIsDark())),
					languageRef.current.of(language === 'erlang' ? erlangLanguage : []),
				],
			}),
			parent: hostRef.current,
		});
		viewRef.current = view;
		const unwatch = watchColorScheme((dark) => {
			view.dispatch({ effects: themeCompartment.reconfigure(themeExtensions(dark)) });
		});
		return () => {
			unwatch();
			view.destroy();
			viewRef.current = null;
		};
		// Mount once; later prop changes are dispatched below.
		// eslint-disable-next-line react-hooks/exhaustive-deps
	}, []);

	useEffect(() => {
		const view = viewRef.current;
		if (!view) return;
		const current = view.state.doc.toString();
		if (current === code) return;
		view.dispatch({ changes: { from: 0, to: current.length, insert: code } });
	}, [code]);

	useEffect(() => {
		const view = viewRef.current;
		if (!view) return;
		view.dispatch({
			effects: languageRef.current.reconfigure(language === 'erlang' ? erlangLanguage : []),
		});
	}, [language]);

	return <div ref={hostRef} className={className} />;
}
