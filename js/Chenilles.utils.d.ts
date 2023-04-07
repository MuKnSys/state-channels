///// General-purpose utilities to maybe move somewhere else someday

// table mapping string to value V
export interface StringTable<V> { [key: string]: V; }

// Association table as an array of key-value pairs. Keys must not repeat.
export type Table<K,V> = Array<[key: K, value: V]>;

// Type of cryptographic hash data, typically using keccak256 on Ethereum.
export type Digest = Uint8Array;

// Type of content-addressed pointer to data of type T,
// wherein the pointer is the cryptographic hash of the serialized data
// typically using keccak256 on Ethereum.
export type ContentAddressed<T> = Digest;
