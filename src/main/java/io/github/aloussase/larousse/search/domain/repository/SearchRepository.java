package io.github.aloussase.larousse.search.domain.repository;

import io.github.aloussase.larousse.search.domain.entity.Definition;

import java.util.List;

public interface SearchRepository {
    List<Definition> search(String term);
}
