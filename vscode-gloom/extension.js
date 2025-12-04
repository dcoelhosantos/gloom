const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');
const path = require('path');
const os = require('os');

let client;

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    console.log('Gloom Language Support is now active!');

    // LSP server executable
    const serverExecutable = path.join(os.homedir(), '.local', 'bin', 'gloom-lsp');

    // LSP client options
    const serverOptions = {
        run: { command: serverExecutable, transport: TransportKind.stdio },
        debug: { command: serverExecutable, transport: TransportKind.stdio }
    };

    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'gloom' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.gloom')
        }
    };

    // Create and start LSP client
    client = new LanguageClient(
        'gloomLanguageServer',
        'Gloom Language Server',
        serverOptions,
        clientOptions
    );

    client.start();

    // Register check command (F7)
    let checkCommand = vscode.commands.registerCommand('gloom.check', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor || editor.document.languageId !== 'gloom') {
            vscode.window.showWarningMessage('No Gloom file is currently open');
            return;
        }

        const terminal = vscode.window.createTerminal('Gloom Check');
        terminal.show();
        terminal.sendText(`cabal run haskell -- --check "${editor.document.fileName}"`);
    });

    // Register run command
    let runCommand = vscode.commands.registerCommand('gloom.run', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor || editor.document.languageId !== 'gloom') {
            vscode.window.showWarningMessage('No Gloom file is currently open');
            return;
        }

        const terminal = vscode.window.createTerminal('Gloom Run');
        terminal.show();
        terminal.sendText(`cabal run haskell -- "${editor.document.fileName}"`);
    });

    context.subscriptions.push(checkCommand);
    context.subscriptions.push(runCommand);

    // Show welcome message
    vscode.window.showInformationMessage('🌙 Gloom LSP active! Errors will appear in real-time.');
}

function deactivate() {
    if (client) {
        return client.stop();
    }
}

module.exports = {
    activate,
    deactivate
}
