package generator;

import util.Block;
import java.util.List;
import java.util.stream.Collectors;

public abstract class Generator<T extends Block> {
    public abstract String generate(List<T> blocks);

    protected String generateImports(List<String> packages) {
        return packages.stream().map(x -> "import " + x + ";").collect(Collectors.joining("\n"));
    }

}
