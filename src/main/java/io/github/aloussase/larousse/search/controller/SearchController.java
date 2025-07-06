package io.github.aloussase.larousse.search.controller;

import io.github.aloussase.larousse.search.domain.service.SearchService;
import io.github.aloussase.larousse.search.dto.DefinitionDto;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/search")
public class SearchController {

    private final SearchService searchService;

    public SearchController(SearchService searchService) {
        this.searchService = searchService;
    }

    @GetMapping
    public ResponseEntity<?> search(@RequestParam("q") String searchTerm) {
        final var defs = searchService.search(searchTerm);
        final var res = defs.stream().map(x ->
                DefinitionDto.builder()
                        .definition(x.getDefinition())
                        .example(x.getExample())
                        .build()
        );
        return ResponseEntity.ok(res);
    }

}
