# Plotter (Rust Edition)

Ce répertoire contient la version Rust de l'application Plotter.

## Prérequis

Pour compiler ce projet, vous devez avoir Rust et Cargo installés sur votre système.
Si ce n'est pas le cas, vous pouvez les installer via [rustup](https://rustup.rs/) :

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Dépendances système (Linux)
Comme ce projet utilise `eframe` (pour l'interface graphique egui), certaines bibliothèques système peuvent être nécessaires sous Linux pour la compilation. Par exemple, sur des distributions basées sur Ubuntu/Debian :
```bash
sudo apt-get install -y libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev libxkbcommon-dev libssl-dev
```

## Compilation

Ouvrez un terminal dans le dossier `rust/` et exécutez la commande suivante pour compiler l'application (mode développement) :

```bash
cargo build
```

Pour compiler l'application avec les optimisations (mode release) afin d'obtenir de meilleures performances, utilisez :

```bash
cargo build --release
```
L'exécutable compilé se trouvera dans le dossier `target/release/` sous le nom `plotter_rs`.

## Exécution

Pour compiler et exécuter l'application en une seule étape :

```bash
cargo run
```

Pour l'exécuter en mode release (recommandé pour une utilisation fluide) :

```bash
cargo run --release
```
