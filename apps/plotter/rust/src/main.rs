use eframe::egui;
use egui_plot::{Line, Plot, PlotPoints};

fn main() -> eframe::Result<()> {
    // Configuration de la fenêtre native
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([800.0, 450.0]) // Ratio 16/9
            .with_title("Tracé multi-fonctions (Rust / egui)"),
        ..Default::default()
    };

    eframe::run_native(
        "Tracé multi-fonctions",
        options,
        Box::new(|_cc| Box::<PlotterApp>::default()),
    )
}

#[derive(Default)]
struct PlotterApp {}

impl eframe::App for PlotterApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            // Configuration des données
            let n = 150;
            let xmin = -1.5;
            let xmax = 10.0;
            let dx = (xmax - xmin) / (n as f64);

            let mut data1 = vec![];
            let mut data2 = vec![];

            // Remplissage des points pour les deux fonctions
            for i in 0..=n {
                let x = xmin + (i as f64) * dx;
                let y1 = (2.0 * x).sin() + (4.0 * x).cos();
                let y2 = x.sin() * 1.5;
                data1.push([x, y1]);
                data2.push([x, y2]);
            }

            // Création de la première courbe (Bleu Indigo)
            let line1 = Line::new(PlotPoints::new(data1))
                .color(egui::Color32::from_rgb(0x25, 0x63, 0xEB))
                .width(3.0)
                .name("fct(x) = sin(2x) + cos(4x)");

            // Création de la deuxième courbe (Orange Vif)
            let line2 = Line::new(PlotPoints::new(data2))
                .color(egui::Color32::from_rgb(0xEA, 0x58, 0x0C))
                .width(3.0)
                .name("fct2(x) = sin(x) * 1.5");

            // Affichage du graphique interactif
            Plot::new("plot")
                .legend(egui_plot::Legend::default())
                .x_axis_label("Axe X")
                .y_axis_label("Axe Y")
                .show(ui, |plot_ui| {
                    plot_ui.line(line1);
                    plot_ui.line(line2);
                });
        });
    }
}
