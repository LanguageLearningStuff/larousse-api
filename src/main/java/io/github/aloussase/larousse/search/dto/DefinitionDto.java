package io.github.aloussase.larousse.search.dto;

import lombok.Builder;
import lombok.Data;

import java.util.Optional;

@Data
@Builder
public class DefinitionDto {
    private String definition;
    private Optional<String> example;
}
