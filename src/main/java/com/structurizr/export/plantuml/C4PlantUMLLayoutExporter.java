package com.structurizr.export.plantuml;

import com.structurizr.Workspace;
import com.structurizr.dsl.IdentifiersRegister;
import com.structurizr.export.Diagram;
import com.structurizr.export.IndentingWriter;
import com.structurizr.export.plantuml.C4PlantUMLExporter;
import com.structurizr.model.*;
import com.structurizr.util.StringUtils;
import com.structurizr.view.RelationshipView;
import com.structurizr.view.View;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import static java.lang.String.format;

public class C4PlantUMLLayoutExporter {
    public final Collection<Diagram> exportWithLayout(Workspace workspace, IdentifiersRegister identifiersRegister) {
        LayoutExporter exporter = new LayoutExporter(identifiersRegister);
        return exporter.export(workspace);
    }

    private class LayoutExporter extends C4PlantUMLExporter {
        private IdentifiersRegister identifiersRegister;

        public LayoutExporter(IdentifiersRegister identifiersRegister) {
            this.identifiersRegister = identifiersRegister;
        }

        protected void writeRelationship(View view, RelationshipView relationshipView, IndentingWriter writer) {
            Relationship relationship = relationshipView.getRelationship();
            Element source = relationship.getSource();
            Element destination = relationship.getDestination();

            if (source instanceof CustomElement || destination instanceof CustomElement) {
                return;
            }

            if (Boolean.TRUE.toString().equalsIgnoreCase(view.getViewSet().getConfiguration().getProperties().getOrDefault(C4PLANTUML_RELATIONSHIP_PROPERTIES_PROPERTY, Boolean.FALSE.toString()))) {
                addProperties(view, writer, relationship);
            }

            if (relationshipView.isResponse() != null && relationshipView.isResponse()) {
                source = relationship.getDestination();
                destination = relationship.getSource();
            }

            String description = "";

            if (!StringUtils.isNullOrEmpty(relationshipView.getOrder())) {
                description = relationshipView.getOrder() + ". ";
            }

            description += (hasValue(relationshipView.getDescription()) ? relationshipView.getDescription() : hasValue(relationshipView.getRelationship().getDescription()) ? relationshipView.getRelationship().getDescription() : "");

            String relationCommand = "Rel_D";
            Map<String, String> props = view.getProperties();
            if (props != null && props.size() > 0) {
                Map<String, String> lowerProps = new HashMap<>(props.size());
                for (Map.Entry<String, String> entry : props.entrySet()) {
                    // Put lowercase values since the identifiersRegister only contains lowercase values
                    lowerProps.put(entry.getKey().toLowerCase(), entry.getValue());
                }
                String sourceIdentifier = this.identifiersRegister.findIdentifier(source);
                String destinationIdentifier = this.identifiersRegister.findIdentifier(destination);
                String directionKey = format("direction_%s_%s", sourceIdentifier, destinationIdentifier);
                String directionOverride = lowerProps.get(directionKey);
                if (directionOverride != null) {
                    if (directionOverride.equalsIgnoreCase("left")) {
                        relationCommand = "Rel_L";
                    } else if (directionOverride.equalsIgnoreCase("right")) {
                        relationCommand = "Rel_R";
                    } else if (directionOverride.equalsIgnoreCase("up")) {
                        relationCommand = "Rel_U";
                    } else if (directionOverride.equalsIgnoreCase("down")) {
                        relationCommand = "Rel_D";
                    }
                }
            }

            if (StringUtils.isNullOrEmpty(relationship.getTechnology())) {
                writer.writeLine(format("%s(%s, %s, \"%s\", $tags=\"%s\")", relationCommand, idOf(source), idOf(destination), description, tagsOf(view, relationship)));
            } else {
                writer.writeLine(format("%s(%s, %s, \"%s\", \"%s\", $tags=\"%s\")", relationCommand, idOf(source), idOf(destination), description, relationship.getTechnology(), tagsOf(view, relationship)));
            }
        }

        private void addProperties(View view, IndentingWriter writer, ModelItem element) {
            Map<String, String> properties = element.getProperties();
            if (!properties.isEmpty()) {
                writer.writeLine("WithoutPropertyHeader()");
                properties.keySet().stream().sorted().forEach(key ->
                        writer.writeLine(String.format("AddProperty(\"%s\",\"%s\")", key, properties.get(key)))
                );
            }
        }

        private String tagsOf(View view, Relationship relationship) {
            if (includeTags(view)) {
                return view.getViewSet().getConfiguration().getStyles().findRelationshipStyle(relationship).getTag().replaceFirst("Relationship,", "");
            } else {
                return "";
            }
        }
    }
}
