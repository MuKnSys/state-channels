// High-level State Channel API
import {Table} from './Chenilles.utils';
import {Petname, NamedAssets} from './Chenilles.names';
import {ChenilleContext} from './Chenilles.shell';

export interface Chenille {}

// Open a simple state channel between two known participants,
// wherein the initial balances will match for each petnamed participant the named assets.
export async function stateChannelOpen(ctx: ChenilleContext, stakes: Table<Petname, NamedAssets>): Chenille;

// Deposit, Withdraw, etc.
export async function stateChannelDeposit (c: Chenille, assets: NamedAssets): boolean;
export async function stateChannelWithdraw (c: Chenille, assets: NamedAssets): boolean;
export async function stateChannelClose (c: Chenille): boolean;

// Send a micropayment on a Chenille State Channel
export async function stateChannelSend (c: Chenille, to: Petname, assets: NamedAssets): boolean;
