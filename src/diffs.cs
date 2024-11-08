// LanguageParser.cs:11190
        private (SyntaxKind operatorTokenKind, SyntaxKind operatorExpressionKind) GetOperatorTokenAndExpressionKind()
        {
            // If the set of expression continuations is updated here, please review ParseStatementAttributeDeclarations
            // to see if it may need a similar look-ahead check to determine if something is a collection expression versus
            // an attribute.

            var token1 = this.CurrentToken;
            var token1Kind = token1.ContextualKind;

            // Merge two consecutive dots into a DotDotToken
            if (IsAtDotDotToken())
                return (SyntaxKind.DotDotToken, SyntaxKind.RangeExpression);

            // check for >>, >>=, >>> or >>>=
            //
            // In all those cases, update token1Kind to be the merged token kind.  It will then be handled by the code below.
            if (token1Kind == SyntaxKind.GreaterThanToken
                && this.PeekToken(1) is { Kind: SyntaxKind.GreaterThanToken or SyntaxKind.GreaterThanEqualsToken } token2
                && NoTriviaBetween(token1, token2)) // check to see if they really are adjacent
            {
                if (token2.Kind == SyntaxKind.GreaterThanToken)
                {
                    if (this.PeekToken(2) is { Kind: SyntaxKind.GreaterThanToken or SyntaxKind.GreaterThanEqualsToken } token3
                        && NoTriviaBetween(token2, token3)) // check to see if they really are adjacent
                    {
                        // >>>  or  >>>=
                        token1Kind = token3.Kind == SyntaxKind.GreaterThanToken
                            ? SyntaxKind.GreaterThanGreaterThanGreaterThanToken
                            : SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken;
                    }
                    else
                    {
                        // >>
                        token1Kind = SyntaxKind.GreaterThanGreaterThanToken;
                    }
                }
                else
                {
                    // >>=
                    token1Kind = SyntaxKind.GreaterThanGreaterThanEqualsToken;
                }
            }

            if (IsExpectedBinaryOperator(token1Kind))
                return (token1Kind, SyntaxFacts.GetBinaryExpression(token1Kind));

            if (IsExpectedAssignmentOperator(token1Kind))
                return (token1Kind, SyntaxFacts.GetAssignmentExpression(token1Kind));

            if (token1Kind == SyntaxKind.SwitchKeyword && this.PeekToken(1).Kind == SyntaxKind.OpenBraceToken)
                return (token1Kind, SyntaxKind.SwitchExpression);

            if (token1Kind == SyntaxKind.WithKeyword && this.PeekToken(1).Kind == SyntaxKind.OpenBraceToken)
                return (token1Kind, SyntaxKind.WithExpression);

            // Something that doesn't expand the current expression we're looking at.  Bail out and see if we
            // can end with a conditional expression.
            return (SyntaxKind.None, SyntaxKind.None);
        }

        private SyntaxToken EatOperatorToken(SyntaxKind operatorTokenKind)
        {
            // Combine tokens into a single token if needed

            if (operatorTokenKind is SyntaxKind.DotDotToken)
                return EatDotDotToken();

            if (operatorTokenKind is SyntaxKind.GreaterThanGreaterThanToken or SyntaxKind.GreaterThanGreaterThanEqualsToken)
            {
                // >> and >>=
                // Two tokens need to be consumed here.

                var token1 = EatToken();
                var token2 = EatToken();

                return SyntaxFactory.Token(
                    token1.GetLeadingTrivia(),
                    operatorTokenKind,
                    token2.GetTrailingTrivia());
            }
            else if (operatorTokenKind is SyntaxKind.GreaterThanGreaterThanGreaterThanToken or SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken)
            {
                // >>> and >>>=
                // Three tokens need to be consumed here.

                var token1 = EatToken();
                _ = EatToken();
                var token3 = EatToken();

                return SyntaxFactory.Token(
                    token1.GetLeadingTrivia(),
                    operatorTokenKind,
                    token3.GetTrailingTrivia());
            }
            else
            {
                // Normal operator.  Eat as a single token, converting contextual words cases (like 'with') to a keyword.
                return this.EatContextualToken(operatorTokenKind);
            }
        }

// LanguageParser.cs:11291
        private AssignmentExpressionSyntax ParseAssignmentOperator(SyntaxKind operatorExpressionKind, ExpressionSyntax leftOperand, SyntaxToken operatorToken)
        {
            Debug.Assert(IsExpectedAssignmentOperator(operatorToken.Kind));
            Debug.Assert(GetPrecedence(operatorExpressionKind) == Precedence.Assignment);

            ExpressionSyntax rhs;
            if (operatorExpressionKind == SyntaxKind.SimpleAssignmentExpression && CurrentToken.Kind == SyntaxKind.RefKeyword &&
                // check for lambda expression with explicit ref return type: `ref int () => { ... }`
                !this.IsPossibleLambdaExpression(Precedence.Assignment))
            {
                rhs = _syntaxFactory.RefExpression(
                    this.EatToken(),
                    this.ParseExpressionCore());
            }
            else
            {
                rhs = this.ParseSubExpression(Precedence.Assignment);
            }

            return _syntaxFactory.AssignmentExpression(
                operatorExpressionKind, leftOperand, operatorToken, rhs);
        }