package io.github.aloussase.larousse.search.infrastructure;

import io.github.aloussase.larousse.search.config.SearchConfig;
import io.github.aloussase.larousse.search.domain.entity.Definition;
import io.github.aloussase.larousse.search.domain.repository.SearchRepository;
import lombok.extern.slf4j.Slf4j;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;
import org.springframework.stereotype.Repository;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

@Slf4j
@Repository
public class JsoupSearchRepository implements SearchRepository {
    private final SearchConfig config;

    public JsoupSearchRepository(SearchConfig config) {
        this.config = config;
    }

    @Override
    public List<Definition> search(String term) {
        final var searchUrl = config.getLarousseBaseUrl() + "/" + term;
        try {
            final var doc = Jsoup.connect(searchUrl).get();
            final var roots = doc.select("li.DivisionDefinition");
            return roots.stream()
                    .map(el -> {
                        final var def = simpleDef(el).trim();
                        final var ex = example(el).map(String::trim);
                        return Definition.builder().definition(def).example(ex).build();
                    })
                    .toList();
        } catch (IOException ex) {
            log.error("There was an error while fetching {}", searchUrl, ex);
            return List.of();
        }
    }

    private String simpleDef(Element el) {
        return el.ownText();
    }

    private Optional<String> example(Element el) {
        return Optional.ofNullable(el.children().selectFirst("span.ExempleDefinition"))
                .map(Element::text);
    }
}
