package io.github.aloussase.larousse.search.domain.service;

import io.github.aloussase.larousse.core.exception.DomainException;
import io.github.aloussase.larousse.search.domain.entity.Definition;
import io.github.aloussase.larousse.search.domain.repository.SearchRepository;

import java.util.List;

public class SearchService {

    private final SearchRepository searchRepository;

    public SearchService(SearchRepository searchRepository) {
        this.searchRepository = searchRepository;
    }

    public List<Definition> search(String term) {
        if (term.strip().isEmpty()) {
            throw new DomainException("The search term must not be empty");
        }

        return searchRepository.search(term);
    }
}
