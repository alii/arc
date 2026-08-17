import { useSyncExternalStore } from 'react';

/** True while `query` matches; re-renders when it flips. */
export function useMediaQuery(query: string): boolean {
	return useSyncExternalStore(
		(onChange) => {
			const mq = window.matchMedia(query);
			mq.addEventListener('change', onChange);
			return () => mq.removeEventListener('change', onChange);
		},
		() => window.matchMedia(query).matches,
		() => false,
	);
}
