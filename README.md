#  structurizr-c4plantuml-layout-exporter

This project is about being able to add some functionality to the structirizr PlantUML C4 export, like for example being able to add some layout options between elements.

For example, in the DSL you could do this
```
container mySystem "Containers" {
    include *
    autoLayout
    properties {
        direction_container1_container2 right
    }
}
```
and it would make the relationshiop from container1 to container2 exit on the right of container1.

## Quick start

1. Pull the docker image
```bash
docker pull matdurand/structurizr-c4plantuml-layout-exporter:latest
```

2. Use the docker image
```bash
docker run -v "/myStructurizrModel:/workspace" matdurand/structurizr-c4plantuml-layout-exporter:latest /workspace/workspace.dsl /workspace/generated
```

## Why?

By default, Structurizr doesn't care much about layout, because if you use Structurizr online or Structurizr Lite, you can adjust the diagrams manually and the positions would be recorded.

I wanted to use Structurizr but still use the auto-layout and have a bit more control over the layout, hence this small POC was born.


## How

It works in 2 steps

1. Read the DSL and create a workspace. We need the DSL, because when the DSL is parsed, there is identity table being build with the name of the elements referenced in the DSL. When we reach the exporter, these names are gone, but we need them because there are going to be referenced in the `properties` of the diagram.
2. Pass both the workspace and identity table to the modified C4 PlantUML exporter. When rendering a relationshop, the exporter will look for some properties in the view to change the layout options.

