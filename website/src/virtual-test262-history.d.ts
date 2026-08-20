declare module 'virtual:test262-history' {
	const history: {
		date: string;
		pass: number;
		fail: number;
		skip: number;
		tested: number;
		total: number;
		percent: number;
	}[];
	export default history;
}
