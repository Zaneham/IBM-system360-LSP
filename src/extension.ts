import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    // Get Python path from configuration
    const config = workspace.getConfiguration('system360');
    const pythonPath = config.get<string>('pythonPath', 'python');

    // Get server path - use bundled if not specified
    let serverPath = config.get<string>('serverPath', '');
    if (!serverPath) {
        serverPath = context.asAbsolutePath(
            path.join('server', 'system360_lsp_server.py')
        );
    }

    // Server options - run Python LSP server
    const serverOptions: ServerOptions = {
        run: {
            command: pythonPath,
            args: [serverPath],
            transport: TransportKind.stdio
        },
        debug: {
            command: pythonPath,
            args: [serverPath, '--debug'],
            transport: TransportKind.stdio
        }
    };

    // Client options - support both COBOL F and PL/I F
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'cobol360' },
            { scheme: 'file', language: 'pli360' }
        ],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/*.{cob,cbl,cobol,cpy,pli,pl1,plinc,inc}')
        }
    };

    // Create and start the client
    client = new LanguageClient(
        'system360',
        'IBM System/360 Languages Server',
        serverOptions,
        clientOptions
    );

    // Start the client
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
