package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.entity.Recipe;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;

/** Dev V3 access matrix용 좁은 projection만 노출하는 dev 전용 repository. */
@Repository
public interface DevRecipeAccessProjectionRepository extends JpaRepository<Recipe, Long> {

    // originRecipe는 nullable FK라 implicit join(r.originRecipe.id)은 inner join이 되어
    // 원본 레시피(originRecipe IS NULL)가 결과에서 누락된다. LEFT JOIN 필수.
    @Query("""
            SELECT new com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection(
                r.id, r.user.id, r.lifecycleStatus, r.visibility, r.listingStatus, r.imageStatus, orig.id)
            FROM Recipe r
            LEFT JOIN r.originRecipe orig
            WHERE r.id IN :ids
            """)
    List<DevRecipeAccessProjection> findAccessProjectionsByIds(@Param("ids") Collection<Long> ids);
}
