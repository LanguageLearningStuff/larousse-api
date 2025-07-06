package io.github.aloussase.larousse.search.domain.entity;

import lombok.Builder;
import lombok.Data;

import java.util.Optional;

@Data
@Builder
public class Definition {
    private String definition;
    private Optional<String> example;
}
