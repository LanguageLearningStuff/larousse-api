package io.github.aloussase.larousse.search.infrastructure;

import io.github.aloussase.larousse.search.config.SearchConfig;
import io.github.aloussase.larousse.search.domain.entity.Definition;
import io.github.aloussase.larousse.search.domain.repository.SearchRepository;
import org.jsoup.Jsoup;
import org.jsoup.nodes.TextNode;
import org.springframework.stereotype.Repository;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

@Repository("macacoSearch")
public class MacacoSearchRepository implements SearchRepository {
    private final SearchConfig config;

    public MacacoSearchRepository(SearchConfig config) {
        this.config = config;
    }

    @Override
    public List<Definition> search(String term) {
        final var searchUrl = config.getMacacoBaseUrl() + "/" + term;
        try {
            final var doc = Jsoup.connect(searchUrl).get();
            final var root = doc.selectFirst("div.verbete.bs-component");
            final var roots = root.textNodes();
            return roots.stream()
                    .filter(tn -> !tn.isBlank())
                    .map(TextNode::text)
                    .map(d -> new Definition(d, Optional.empty()))
                    .toList();
        } catch (IOException ex) {
            return List.of();
        }
    }
}
