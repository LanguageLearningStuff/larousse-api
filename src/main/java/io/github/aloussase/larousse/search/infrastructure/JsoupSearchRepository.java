package io.github.aloussase.larousse.search.infrastructure;

import io.github.aloussase.larousse.search.config.SearchConfig;
import io.github.aloussase.larousse.search.domain.entity.Definition;
import io.github.aloussase.larousse.search.domain.repository.SearchRepository;
import io.github.resilience4j.retry.Retry;
import lombok.extern.slf4j.Slf4j;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Slf4j
@Repository("larousseSearch")
public class JsoupSearchRepository implements SearchRepository {
    private final SearchConfig config;
    private final Retry retry;

    public JsoupSearchRepository(SearchConfig config, Retry retry) {
        this.config = config;
        this.retry = retry;
    }

    @Override
    public List<Definition> search(String term) {
        final var searchUrl = config.getLarousseBaseUrl() + "/" + term;
        try {
            final var getDoc = Retry.decorateCheckedSupplier(
                    retry,
                    () -> Jsoup.connect(searchUrl).get());
            final var doc = getDoc.get();
            final var roots = doc.select("li.DivisionDefinition");
            return roots.stream()
                    .map(el -> {
                        final var def = simpleDef(el).trim();
                        final var ex = example(el).map(String::trim);
                        return Definition.builder().definition(def).example(ex).build();
                    })
                    .toList();
        } catch (Throwable ex) {
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
