package generator;

import util.Import;
import java.util.List;
import java.util.stream.Collectors;

public class ImportGen extends Generator<Import> {

    @Override
    public String generate(List<Import> blocks) {
        return generateImports(blocks.stream().map(Import::toJava).collect(Collectors.toList()));
    }
}
