package com.github.matdurand;

import com.structurizr.export.plantuml.C4PlantUMLEnhancedExporter;
import com.structurizr.Workspace;
import com.structurizr.dsl.StructurizrDslParser;
import com.structurizr.export.Diagram;

import java.io.BufferedWriter;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collection;

public class StructurizrC4PlantUMLExporter {
    public static void main(String[] args) {

        if (args.length < 2 || args.length > 3) {
            System.out.println("Usage: StructC4LayoutExporter <structurizr workspace dsl file> <output path> [file prefix]");
            System.exit(0);
        }

        File workspaceFile = new File(args[0]);
        if (!workspaceFile.exists()) {
            System.err.println(args[0] + " doesn't exist");
            System.exit(1);
        }

        File outputPath = new File(args[1]);
        if (outputPath.exists()) {
            if (outputPath.isFile()) {
                System.err.println(args[1] + " should be a folder, not a file");
                System.exit(1);
            }
        } else {
            if (!outputPath.mkdirs()) {
                System.err.println(args[1] + " could not be created");
                System.exit(1);
            }
        }

        String prefix = "";
        if (args.length == 3) {
            prefix = args[2];
        }

        try {
            dslToLayoutC4Plant(workspaceFile, outputPath, prefix);
        } catch(Throwable t) {
            System.err.println(t);
            System.exit(1);
        }
    }

    private static void dslToLayoutC4Plant(File workspaceFile, File outputPath, String prefix) throws Exception {
        StructurizrDslParser parser = new StructurizrDslParser();
        parser.parse(workspaceFile);

        Workspace workspace = parser.getWorkspace();
        if (workspace.getViews().isEmpty()) {
            System.out.println(" - the workspace contains no views");
            System.exit(1);
        }

        C4PlantUMLEnhancedExporter exporter = new C4PlantUMLEnhancedExporter();
        Collection<Diagram> diagrams = exporter.exportWithLayout(parser.getWorkspace(), parser.getIdentifiersRegister());

        for (Diagram diagram : diagrams) {
            File file = new File(outputPath, String.format("%s%s.%s", prefix, diagram.getKey(), diagram.getFileExtension()));
            writeToFile(file, diagram.getDefinition());
        }
    }

    private static String prefix(long workspaceId) {
        if (workspaceId > 0) {
            return "structurizr-" + workspaceId;
        } else {
            return "structurizr";
        }
    }

    private static void writeToFile(File file, String content) throws Exception {
        System.out.println(" - writing " + file.getCanonicalPath());

        BufferedWriter writer = Files.newBufferedWriter(file.toPath(), StandardCharsets.UTF_8);
        writer.write(content);
        writer.flush();
        writer.close();
    }
}