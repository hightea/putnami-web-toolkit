/**
 * This file is part of pwt-code-editor.
 *
 * pwt-code-editor is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-code-editor is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-code-editor.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.code.client.configuration.java;

import java.util.List;

import com.google.common.collect.Lists;

import fr.putnami.pwt.plugin.code.client.aspect.CodeEditorAspect;
import fr.putnami.pwt.plugin.code.client.assist.CodeContentAssistAspect;
import fr.putnami.pwt.plugin.code.client.configuration.CodeEditorConfiguration;
import fr.putnami.pwt.plugin.code.client.render.CssRendererTokenContent;
import fr.putnami.pwt.plugin.code.client.render.TextRendererAspect;
import fr.putnami.pwt.plugin.code.client.token.evaluator.EndOfLineTokenEvaluator;
import fr.putnami.pwt.plugin.code.client.token.evaluator.KeywordsTokenEvaluator;
import fr.putnami.pwt.plugin.code.client.token.evaluator.MultiLineTokenEvaluator;
import fr.putnami.pwt.plugin.code.client.token.evaluator.SingleLineTokenEvaluator;

public class JavaConfiguration implements CodeEditorConfiguration {

	public static final JavaConfiguration JAVA_CONFIGURATION = new JavaConfiguration();

	private List<CodeEditorAspect> aspects = Lists.newArrayList();

	private JavaConfiguration() {
		TextRendererAspect renderAspect = new TextRendererAspect();

		KeywordsTokenEvaluator wordTokenEvaluator = new KeywordsTokenEvaluator(CssRendererTokenContent.DEFAULT_CSS_TOKEN_CONTENT);
		wordTokenEvaluator.addWords(new CssRendererTokenContent("code-editor-java-keyword"), "abstract", "assert", "boolean", "break", "byte", "case",
				"catch", "char", "class", "const", "continue", "default", "do", "double", "else", "enum", "extends", "false", "final", "finally",
				"float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", "package",
				"private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
				"transient", "true", "try", "void", "volatile", "while");

		renderAspect.registerEvaluator(new SingleLineTokenEvaluator("\"", "\"", new CssRendererTokenContent("code-editor-java-string"), '\\'));
		renderAspect.registerEvaluator(new SingleLineTokenEvaluator("'", "'", new CssRendererTokenContent("code-editor-java-string"), '\\'));
		renderAspect.registerEvaluator(new MultiLineTokenEvaluator("/**", "*/", new CssRendererTokenContent("code-editor-java-doc"), '\\', true));
		renderAspect.registerEvaluator(new MultiLineTokenEvaluator("/*", "*/", new CssRendererTokenContent("code-editor-java-multi-line-comment"),
				'\\', true));
		renderAspect.registerEvaluator(new EndOfLineTokenEvaluator("//", new CssRendererTokenContent("code-editor-java-single-line-comment")));
		renderAspect.registerEvaluator(new EndOfLineTokenEvaluator("@", new CssRendererTokenContent("code-editor-java-annotation")));
		renderAspect.registerEvaluator(wordTokenEvaluator);
		aspects.add(renderAspect);

		CodeContentAssistAspect assistAspect = new CodeContentAssistAspect(new JavaKeywordAssistHandler());
		aspects.add(assistAspect);
	}

	@Override
	public List<CodeEditorAspect> getAspects() {
		return aspects;
	}
}
