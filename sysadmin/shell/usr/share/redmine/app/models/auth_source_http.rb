class AuthSourceHTTP < AuthSource
    def authenticate(login, password)
        login = ENV["REMOTE_USER"]

        i = login.index("@")
        if i
            firstname = login[0, i]
            mail = firstname + login[i, login.length].downcase
        else
            firstname = login
            mail = nil
        end

        if (onthefly_register?)
            return [
                :firstname      => firstname,
                :lastname       => nil,
                :mail           => mail,
                :auth_source_id => self.id
            ]
        else
            return firstname
        end
    end

    def auth_method_name
        "HTTP"
    end
end
