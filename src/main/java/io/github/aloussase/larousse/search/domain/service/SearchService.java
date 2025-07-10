package io.github.aloussase.larousse.search.domain.service;

import io.github.aloussase.larousse.core.exception.DomainException;
import io.github.aloussase.larousse.search.domain.entity.Definition;
import io.github.aloussase.larousse.search.domain.repository.SearchRepository;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

public class SearchService {

    public List<Definition> search(SearchRepository searchRepository, String term) {
        if (term.strip().isEmpty()) {
            throw new DomainException("The search term must not be empty");
        }

        final var defs = searchRepository.search(term);

        return defs.stream()
                .filter(def -> def.getDefinition().length() > 3)
                .filter(def -> Arrays.stream(def.getDefinition().split("\\s+"))
                        .filter(Predicate.not(String::isBlank))
                        .count() > 2)
                .filter(def -> !def.getDefinition().startsWith(","))
                .map(def -> new Definition(def.getDefinition().strip(), def.getExample()))
                .toList();
    }
}
